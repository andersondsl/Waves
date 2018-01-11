package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.TransferTransactionOLD

class TransferTransactionOLDSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferGen) { transfer: TransferTransactionOLD =>
      require(transfer.bytes().head == TransactionType.TransferTransactionOLD.id)
      val recovered = TransferTransactionOLD.parseTail(transfer.bytes().tail).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.assetId.map(_ == transfer.assetId.get).getOrElse(transfer.assetId.isEmpty) shouldBe true
      recovered.feeAssetId.map(_ == transfer.feeAssetId.get).getOrElse(transfer.feeAssetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferGen) { tx: TransferTransactionOLD =>
      val recovered = TransactionParser.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

}
