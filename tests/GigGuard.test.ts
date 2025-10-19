
import { describe, expect, it } from "vitest";
import { Simnet } from "@hirosystems/clarinet-sdk";
import { Cl } from "@stacks/transactions";

// @ts-ignore - This is injected by the Clarinet test runner
declare const simnet: Simnet;

const accounts = simnet.getAccounts();
const client = accounts.get("wallet_1")!;
const freelancer = accounts.get("wallet_2")!;

describe("GigGuard contract tests", () => {
  // Helper function to create a job
  const createJob = () => {
    return simnet.callPublicFn(
      "GigGuard",
      "create-job",
      [
        Cl.principal(freelancer),
        Cl.uint(1000000), // 1 STX in microSTX
        Cl.stringAscii('STX'),
        Cl.uint(2),
        Cl.stringAscii('Test Job')
      ],
      client
    );
  };

  // Helper function to deposit into escrow
  const depositEscrow = (jobId: number, amount: number, caller: string) => {
    return simnet.callPublicFn(
      "GigGuard",
      "deposit-escrow",
      [
        Cl.uint(jobId),
        Cl.uint(amount)
      ],
      caller
    );
  };

  describe("deposit-escrow function", () => {
    it("should allow client to deposit funds into escrow", () => {
      // Create a job first
      const jobResult = createJob();
      expect(jobResult.result).toBeOk(Cl.uint(0)); // First job ID should be 0
      
      // Deposit funds
      const depositAmount = 500000; // 0.5 STX
      const depositResult = depositEscrow(0, depositAmount, client);
      
      // Check if deposit was successful
      expect(depositResult.result).toBeOk(Cl.bool(true));
    });

    it("should fail if called with wrong number of arguments", () => {
      // This test verifies that the function can't be called with wrong number of args
      const invalidCall = () => {
        simnet.callPublicFn(
          "GigGuard",
          "deposit-escrow",
          [
            Cl.uint(1),
            Cl.uint(100000),
            Cl.stringAscii('extra-arg') // Extra argument
          ],
          client
        );
      };
      
      expect(invalidCall).toThrow();
    });
  });
});
