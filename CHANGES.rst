Development Changelog
'''''''''''''''''''''

**NB:** The changelog for releases can be found at: https://tezos.gitlab.io/CHANGES.html


This file lists the changes added to each version of octez-node,
octez-client, and the other Octez executables. The changes to the economic
protocol are documented in the ``docs/protocols/`` directory; in
particular in ``docs/protocols/alpha.rst``.

When you make a commit on master, you can add an item in one of the
following subsections (node, client, …) to document your commit or the
set of related commits. This will ensure that this change is not
forgotten in the final changelog, which can be found in ``docs/CHANGES.rst``.
By having your commits update this file you also make it easy to find the
commits which are related to your changes using ``git log -p -- CHANGES.rst``.
Relevant items are moved to ``docs/CHANGES.rst`` after each release.

Only describe changes which affect users (bug fixes and new features),
or which will affect users in the future (deprecated features),
not refactorings or tests. Changes to the documentation do not need to
be documented here either.

General
-------

Node
----

- **Breaking Changes**: Improved a few lib_shell logs in default level by
  shortening the display to completion time only instead of the full status of
  the operation.

- Added an option ``daily-logs`` to file-descriptor sinks, enabling
  log rotation based on a daily frequency.

- Fixed a bug while reconstructing the storage after a snapshot import
  that would result in wrong context hash mapping for some blocks.

- Deprecated the RPC ``GET /monitor/valid_blocks`` and introduced
  ``GET /monitor/validated_blocks`` and ``GET /monitor/applied_blocks``
  which respectively returns validated blocks, which are not yet applied
  nor stored, and applied blocks which are fully applied and stored by
  the node. (MR :gl: `!7513`)

- Replaced some "precheck" occurrences with "validate" in event and
  error identifiers and messages. (MR :gl: `!7513`)

- Added an option ``create-dirs`` to file-descriptor sinks to allow
  the node to create the log directory and its parents if they don't exist.

- Added a default node configuration that enables disk logs as a
  file-descriptor-sink in the data directory of the node with a 7 days rotation.

- **Breaking Change**: Removed section in stdout logs lines

- Removed the ``indexing-strategy`` option from the ``TEZOS_CONTEXT``
  environment variable to prevent the usage of the ``always``
  indexing strategy. For now, only the ``minimal`` indexing strategy
  is allowed.

Client
------

Baker
-----

- Changed the baker default semantics so that it performs a light
  validation of operations to classify them instead of fully applying
  them. Hence, the block production is now more
  time/cpu/disk-efficient. In this mode, application-dependent checks
  are disabled. Setting the ``--force-apply`` flag on the command line
  restores the previous behavior. (MR :gl:`!7490`)

- **Breaking Change**: Disabled the verification of signature of
  operations in the baker when baking a block. The baker must always
  be provided operations with a valid signature, otherwise produced
  blocks will be invalid and rejected by local nodes during their
  injection. Default setups are not affected but external mempools
  should make sure that their operations' signatures are correct.
  (MR :gl:`!7490`)

- Made the baker discard legacy or corrupted Tenderbake's saved
  states in order to avoid unexpected crashes when the baker gets
  updated, or when a new protocol's baker starts. (MR :gl:`!7640`)

- Restored previous behaviour from :gl:`!7490` for blocks at round
  greater than 0. Application-dependent checks are re-enabled for
  re-proposal and fresh blocks at round greater than 0.

- Reduced the preendorsement injection delay by making the baker
  preendorse as soon as the node considers a block as valid instead of
  waiting for the node to fully apply it. (MR :gl:`!7516`)

Accuser
-------

Signer
------

Proxy Server
------------

Protocol Compiler And Environment
---------------------------------

Codec
-----

Docker Images
-------------

Smart Rollup node
-----------------

Smart Rollup client
-------------------

Miscellaneous
-------------
