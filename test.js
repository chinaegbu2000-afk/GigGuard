const { Clarinet } = require('@stacks/clarinet');
const { test, beforeAll, afterAll, afterEach, describe, expect, assert } = require('vitest');

Clarinet.run({
  async fn(chain, accounts) {
    const deployer = accounts.get('deployer');
    const client = accounts.get('wallet_1');
    const freelancer = accounts.get('wallet_2');

    // Test create-job
    let block = chain.mineBlock([
      {
        sender: client,
        contract: 'GigGuard',
        function: 'create-job',
        args: [
          { type: 'principal', value: freelancer },
          { type: 'uint', value: '1000000' },
          { type: 'string-ascii', value: 'STX' },
          { type: 'uint', value: '2' },
          { type: 'string-ascii', value: 'Test Job' }
        ],
      },
    ]);
    
    console.log('Block results:', JSON.stringify(block, null, 2));
    
    // Check if the transaction was successful
    assert.equal(block.receipts.length, 1);
    assert.equal(block.receipts[0].result, '(ok u0)');
  },
});
