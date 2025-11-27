import gleeunit
import gleeunit/should
import utils

pub fn main() -> Nil {
  gleeunit.main()
}

// Test password hashing
pub fn hash_password_test() {
  let password = "test_password"
  let hash = utils.hash_password(password)
  
  // Hash should not equal the password
  should.not_equal(hash, password)
  
  // Same password should produce same hash
  let hash2 = utils.hash_password(password)
  should.equal(hash, hash2)
}

// Test password verification
pub fn verify_password_test() {
  let password = "secret123"
  let hash = utils.hash_password(password)
  
  // Correct password should verify
  should.be_true(utils.verify_password(password, hash))
  
  // Wrong password should not verify
  should.be_false(utils.verify_password("wrong", hash))
}

// Test key pair generation
pub fn generate_key_pair_test() {
  let #(pub_key, priv_key) = utils.generate_key_pair()
  
  // Keys should not be empty
  should.not_equal(pub_key, "")
  should.not_equal(priv_key, "")
}

// Test digital signatures
pub fn sign_and_verify_test() {
  let #(pub_key, priv_key) = utils.generate_key_pair()
  let content = "Hello, World!"
  
  // Sign the content
  let signature = utils.sign_content(content, priv_key)
  
  // Signature should not be empty
  should.not_equal(signature, "")
  
  // Verify should return true for correct signature
  should.be_true(utils.verify_signature(content, signature, pub_key))
  
  // Verify should return false for wrong content
  should.be_false(utils.verify_signature("Wrong content", signature, pub_key))
}

// Test random int generation
pub fn random_int_test() {
  let val = utils.random_int(100)
  
  // Should be within range
  should.be_true(val >= 0)
  should.be_true(val <= 100)
}

// Test contains_lowercase
pub fn contains_lowercase_test() {
  should.be_true(utils.contains_lowercase("Hello World", "world"))
  should.be_true(utils.contains_lowercase("HELLO", "hello"))
  should.be_false(utils.contains_lowercase("Hello", "xyz"))
}

// Test timestamp
pub fn current_timestamp_test() {
  let ts1 = utils.current_timestamp()
  let ts2 = utils.current_timestamp()
  
  // Timestamps should be positive
  should.be_true(ts1 > 0)
  
  // Second timestamp should be >= first
  should.be_true(ts2 >= ts1)
}
