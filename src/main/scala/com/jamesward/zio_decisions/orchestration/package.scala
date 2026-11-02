package com.jamesward.zio_decisions.orchestration

/** Orchestration compares only values within the same statically known type. */
given orchestrationCanEqual[A]: CanEqual[A, A] = CanEqual.derived
