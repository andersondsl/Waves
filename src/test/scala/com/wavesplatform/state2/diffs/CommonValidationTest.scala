package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransactionOLD

class CommonValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, TransferTransactionOLD)] = for {
      master <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      transfer: TransferTransactionOLD <- wavesTransferGeneratorP(master, recipient)
    } yield (genesis, transfer)

    forAll(preconditionsAndPayment) { case ((genesis, transfer)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, transfer))), TestBlock.create(Seq(transfer))) { blockDiffEi =>
        blockDiffEi should produce("AlreadyInTheState")
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer, transfer))) { blockDiffEi =>
        blockDiffEi should produce("AlreadyInTheState")
      }
    }
  }
}
