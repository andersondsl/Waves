package com.wavesplatform.it.transactions

import com.wavesplatform.it.TransferSending
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import scorex.account.{AddressOrAlias, PrivateKeyAccount}
import scorex.api.http.Mistiming
import scorex.transaction.assets.TransferTransactionOLD

import scala.concurrent.Await
import scala.concurrent.Future.{sequence, traverse}
import scala.concurrent.duration._

class TransferTransactionOLDSuite extends BaseTransactionSuite with TransferSending with CancelAfterFailure {

  private val waitCompletion = 2.minutes
  private val defaultAssetQuantity = 100000
  private val transferAmount = 5.waves
  private val leasingAmount = 5.waves
  private val leasingFee = 0.003.waves
  private val transferFee = 0.002.waves
  private val issueFee = 5.waves

  test("asset transfer changes sender's and recipient's asset balance; issuer's.waves balance is decreased by fee") {
    val f = for {
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- accountBalances(firstAddress)
        .zip(accountBalances(secondAddress))

      issuedAssetId <- sender.issue(firstAddress, "name", "description", defaultAssetQuantity, 2, reissuable = false, issueFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)
      _ <- assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
        .zip(assertAssetBalance(firstAddress, issuedAssetId, defaultAssetQuantity))

      TransferTransactionOLDId <- sender.transfer(firstAddress, secondAddress, defaultAssetQuantity, transferFee, Some(issuedAssetId)).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(TransferTransactionOLDId, 1)
      _ <- assertBalances(firstAddress, firstBalance - transferFee - issueFee, firstEffBalance - transferFee - issueFee)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance))
        .zip(assertAssetBalance(firstAddress, issuedAssetId, 0))
        .zip(assertAssetBalance(secondAddress, issuedAssetId, defaultAssetQuantity))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("waves transfer changes waves balances and eff.b.") {
    val f = for {
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- accountBalances(firstAddress)
        .zip(accountBalances(secondAddress))

      transferId <- sender.transfer(firstAddress, secondAddress, transferAmount, transferFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(transferId, 1)
      _ <- assertBalances(firstAddress, firstBalance - transferAmount - transferFee, firstEffBalance - transferAmount - transferFee)
        .zip(assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    def invalidByTsTx(ts: Long) = TransferTransactionOLD.create(None,
      PrivateKeyAccount.fromSeed(sender.accountSeed).right.get,
      AddressOrAlias.fromString(sender.address).right.get,
      1,
      ts,
      None,
      1.waves,
      Array.emptyByteArray
    ).right.get

    val invalidTimestamps: Seq[Long] = Seq(
      System.currentTimeMillis() + 1.day.toMillis,
      1e15.toLong // NODE-416
    )

    for (timestamp <- invalidTimestamps) {
      val tx = invalidByTsTx(timestamp)
      val id = tx.id()
      val req = createSignedTransferRequest(tx)
      val f = for {
        _ <- expectErrorResponse(sender.signedTransfer(req)) { x =>
          x.error == Mistiming.Id
        }
        _ <- sequence(nodes.map(_.ensureTxDoesntExist(id.base58)))
      } yield succeed

      Await.result(f, waitCompletion)
    }
  }

  test("can not make transfer without having enough of fee") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- accountBalances(firstAddress)
        .zip(accountBalances(secondAddress))

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, secondEffBalance, transferFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, firstBalance, firstEffBalance)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }


  test("can not make transfer without having enough of waves") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- accountBalances(firstAddress)
        .zip(accountBalances(secondAddress))

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, secondBalance + 1.waves, transferFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, firstBalance, firstEffBalance)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }

  test("can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- accountBalances(firstAddress)
        .zip(accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(createdLeaseTxId, 1)

      _ <- assertBalances(firstAddress, firstBalance - leasingFee, firstEffBalance - leasingAmount - leasingFee)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      transferFailureAssertion <- assertBadRequest(sender.transfer(firstAddress, secondAddress, firstBalance - leasingFee - transferFee, fee = transferFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, firstBalance - leasingFee, firstEffBalance - leasingAmount - leasingFee)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }

  test("can not make transfer without having enough of your own waves") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- accountBalances(firstAddress)
        .zip(accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = leasingFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(createdLeaseTxId, 1)

      _ <- assertBalances(firstAddress, firstBalance - leasingFee, firstEffBalance - leasingAmount - leasingFee)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      //effecdtive balance is greater than own balance
      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress
        , secondBalance + (secondEffBalance - secondBalance) / 2
        , transferFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, firstBalance - leasingFee, firstEffBalance - leasingAmount - leasingFee)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }

  test("can forge block with sending majority of some asset to self and to other account") {
    val f = for {
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- accountBalances(firstAddress)
        .zip(accountBalances(secondAddress))

      assetId <- sender.issue(firstAddress, "second asset", "description", defaultAssetQuantity, 0, reissuable = false, fee = issueFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(assetId, 1)

      _ <- assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
        .zip(assertAssetBalance(firstAddress, assetId, defaultAssetQuantity))

      tx1 <- sender.transfer(firstAddress, firstAddress, defaultAssetQuantity, fee = transferFee, Some(assetId)).map(_.id)
      tx2 <- sender.transfer(firstAddress, secondAddress, defaultAssetQuantity / 2, fee = transferFee, Some(assetId)).map(_.id)

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      _ <- traverse(nodes)(_.waitForTransaction(tx1))
        .zip(traverse(nodes)(_.waitForTransaction(tx2)))

      _ <- traverse(nodes)(_.waitForHeight(height + 5))

      _ <- assertBalances(firstAddress, firstBalance - issueFee - 2 * transferFee, firstEffBalance - issueFee - 2 * transferFee)
        .zip(assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield succeed

    Await.result(f, waitCompletion)
  }
}
