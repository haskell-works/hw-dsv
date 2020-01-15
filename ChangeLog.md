# Changelog for hw-dsv

## 0.4.0

* Resolve some ambiguity to whether functions are lazy by moving them to designated modules.
* Fix names of functions which were lying about the types they were dealing with.  In particular:

  * ```haskell
    selectListVector :: [Int] -> DsvCursor -> [[LBS.ByteString]]
    ```

    should be

    ```haskell
    selectListList :: [Int] -> DsvCursor -> [[LBS.ByteString]]
    ```

* Remove deprecated functions.

## 0.3.7

* Deprecate some functions.  Some of these functions have been moved to another module or renamed.
  See implementation of deprecated functions to see how downstream code should be refactor.

## 0.2.1

* Add instance NFData Lazy.DsvCursor

## 0.2

* Change delimiter types from Char to Word8, since only 8-bit chars work.
* Support GHC 7.10
* Don't store delimiters in cursors unnecessarily

## Unreleased changes
