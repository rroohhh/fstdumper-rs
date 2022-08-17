use vpi_sys::*;

pub fn vpi_const_to_str(v: PLI_INT32) -> &'static str {
    macro_rules! gen {
        ($v:ident, $($name:ident,)*) => {
            #[allow(non_upper_case_globals)]
            match $v {
                $(
                    $name => stringify!($name),
                )*
                _ => "(unknown)"
            }
        }
    }

    gen! {
        v,
        vpiAlways,
        vpiAssignStmt,
        vpiAssignment,
        vpiBegin,
        vpiCase,
        vpiCaseItem,
        vpiConstant,
        vpiContAssign,
        vpiDeassign,
        vpiDefParam,
        vpiDelayControl,
        vpiDisable,
        vpiEventControl,
        vpiEventStmt,
        vpiFor,
        vpiForce,
        vpiForever,
        vpiFork,
        vpiFuncCall,
        vpiFunction,
        vpiGate,
        vpiIf,
        vpiIfElse,
        vpiInitial,
        vpiIntegerVar,
        vpiInterModPath,
        vpiIterator,
        vpiIODecl,
        vpiMemory,
        vpiMemoryWord,
        vpiModPath,
        vpiModule,
        vpiNamedBegin,
        vpiNamedEvent,
        vpiNamedFork,
        vpiNet,
        vpiNetBit,
        vpiNullStmt,
        vpiOperation,
        vpiParamAssign,
        vpiParameter,
        vpiPartSelect,
        vpiPathTerm,
        vpiPort,
        vpiPortBit,
        vpiPrimTerm,
        vpiRealVar,
        vpiReg,
        vpiRegBit,
        vpiRelease,
        vpiRepeat,
        vpiRepeatControl,
        vpiSchedEvent,
        vpiSpecParam,
        vpiSwitch,
        vpiSysFuncCall,
        vpiSysTaskCall,
        vpiTableEntry,
        vpiTask,
        vpiTaskCall,
        vpiTchk,
        vpiTchkTerm,
        vpiTimeVar,
        vpiTimeQueue,
        vpiUdp,
        vpiUdpDefn,
        vpiUserSystf,
        vpiVarSelect,
        vpiWait,
        vpiWhile,
        vpiAttribute,
        vpiBitSelect,
        vpiCallback,
        vpiDelayTerm,
        vpiDelayDevice,
        vpiFrame,
        vpiGateArray,
        vpiModuleArray,
        vpiPrimitiveArray,
        vpiNetArray,
        vpiRange,
        vpiRegArray,
        vpiSwitchArray,
        vpiUdpArray,
        vpiContAssignBit,
        vpiNamedEventArray,
        vpiIndexedPartSelect,
        vpiGenScopeArray,
        vpiGenScope,
        vpiGenVar,
        vpiCondition,
        vpiDelay,
        vpiElseStmt,
        vpiForIncStmt,
        vpiForInitStmt,
        vpiHighConn,
        vpiLhs,
        vpiIndex,
        vpiLeftRange,
        vpiLowConn,
        vpiParent,
        vpiRhs,
        vpiRightRange,
        vpiScope,
        vpiSysTfCall,
        vpiTchkDataTerm,
        vpiTchkNotifier,
        vpiTchkRefTerm,
        vpiArgument,
        vpiBit,
        vpiDriver,
        vpiInternalScope,
        vpiLoad,
        vpiModDataPathIn,
        vpiModPathIn,
        vpiModPathOut,
        vpiOperand,
        vpiPortInst,
        vpiProcess,
        vpiVariables,
        vpiUse,
        vpiExpr,
        vpiPrimitive,
        vpiStmt,
        vpiActiveTimeFormat,
        vpiInTerm,
        vpiInstanceArray,
        vpiLocalDriver,
        vpiLocalLoad,
        vpiOutTerm,
        vpiPorts,
        vpiSimNet,
        vpiTaskFunc,
        vpiBaseExpr,
        vpiWidthExpr,
        vpiAutomatics,
        vpiUndefined,
        vpiType,
        vpiName,
        vpiFullName,
        vpiSize,
        vpiFile,
        vpiLineNo,
        vpiTopModule,
        vpiCellInstance,
        vpiDefName,
        vpiProtected,
        vpiTimeUnit,
        vpiTimePrecision,
        vpiDefNetType,
        vpiUnconnDrive,
        vpiHighZ,
        vpiPull1,
        vpiPull0,
        vpiDefFile,
        vpiDefLineNo,
        vpiDefDelayMode,
        vpiDelayModeNone,
        vpiDelayModePath,
        vpiDelayModeDistrib,
        vpiDelayModeUnit,
        vpiDelayModeZero,
        vpiDelayModeMTM,
        vpiDefDecayTime,
        vpiScalar,
        vpiVector,
        vpiExplicitName,
        vpiDirection,
        vpiInput,
        vpiOutput,
        vpiInout,
        vpiMixedIO,
        vpiNoDirection,
        vpiConnByName,
        vpiNetType,
        vpiWire,
        vpiWand,
        vpiWor,
        vpiTri,
        vpiTri0,
        vpiTri1,
        vpiTriReg,
        vpiTriAnd,
        vpiTriOr,
        vpiSupply1,
        vpiSupply0,
        vpiNone,
        vpiUwire,
        vpiExplicitScalared,
        vpiExplicitVectored,
        vpiExpanded,
        vpiImplicitDecl,
        vpiChargeStrength,
        vpiArray,
        vpiPortIndex,
        vpiTermIndex,
        vpiStrength0,
        vpiStrength1,
        vpiPrimType,
        vpiAndPrim,
        vpiNandPrim,
        vpiNorPrim,
        vpiOrPrim,
        vpiXorPrim,
        vpiXnorPrim,
        vpiBufPrim,
        vpiNotPrim,
        vpiBufif0Prim,
        vpiBufif1Prim,
        vpiNotif0Prim,
        vpiNotif1Prim,
        vpiNmosPrim,
        vpiPmosPrim,
        vpiCmosPrim,
        vpiRnmosPrim,
        vpiRpmosPrim,
        vpiRcmosPrim,
        vpiRtranPrim,
        vpiRtranif0Prim,
        vpiRtranif1Prim,
        vpiTranPrim,
        vpiTranif0Prim,
        vpiTranif1Prim,
        vpiPullupPrim,
        vpiPulldownPrim,
        vpiSeqPrim,
        vpiCombPrim,
        vpiPolarity,
        vpiDataPolarity,
        vpiPositive,
        vpiNegative,
        vpiUnknown,
        vpiEdge,
        vpiNoEdge,
        vpiEdge01,
        vpiEdge10,
        vpiEdge0x,
        vpiEdgex1,
        vpiEdge1x,
        vpiEdgex0,
        vpiPosedge,
        vpiNegedge,
        vpiAnyEdge,
        vpiPathType,
        vpiPathFull,
        vpiPathParallel,
        vpiTchkType,
        vpiSetup,
        vpiHold,
        vpiPeriod,
        vpiWidth,
        vpiSkew,
        vpiRecovery,
        vpiNoChange,
        vpiSetupHold,
        vpiFullskew,
        vpiRecrem,
        vpiRemoval,
        vpiTimeskew,
        vpiOpType,
        vpiMinusOp,
        vpiPlusOp,
        vpiNotOp,
        vpiBitNegOp,
        vpiUnaryAndOp,
        vpiUnaryNandOp,
        vpiUnaryOrOp,
        vpiUnaryNorOp,
        vpiUnaryXorOp,
        vpiUnaryXNorOp,
        vpiSubOp,
        vpiDivOp,
        vpiModOp,
        vpiEqOp,
        vpiNeqOp,
        vpiCaseEqOp,
        vpiCaseNeqOp,
        vpiGtOp,
        vpiGeOp,
        vpiLtOp,
        vpiLeOp,
        vpiLShiftOp,
        vpiRShiftOp,
        vpiAddOp,
        vpiMultOp,
        vpiLogAndOp,
        vpiLogOrOp,
        vpiBitAndOp,
        vpiBitOrOp,
        vpiBitXorOp,
        vpiBitXNorOp,
        vpiBitXnorOp,
        vpiConditionOp,
        vpiConcatOp,
        vpiMultiConcatOp,
        vpiEventOrOp,
        vpiNullOp,
        vpiListOp,
        vpiMinTypMaxOp,
        vpiPosedgeOp,
        vpiNegedgeOp,
        vpiArithLShiftOp,
        vpiArithRShiftOp,
        vpiPowerOp,
        vpiConstType,
        vpiDecConst,
        vpiRealConst,
        vpiBinaryConst,
        vpiOctConst,
        vpiHexConst,
        vpiStringConst,
        vpiIntConst,
        vpiTimeConst,
        vpiBlocking,
        vpiCaseType,
        vpiCaseExact,
        vpiCaseX,
        vpiCaseZ,
        vpiNetDeclAssign,
        vpiFuncType,
        vpiIntFunc,
        vpiRealFunc,
        vpiTimeFunc,
        vpiSizedFunc,
        vpiSizedSignedFunc,
        vpiSysFuncType,
        vpiSysFuncInt,
        vpiSysFuncReal,
        vpiSysFuncTime,
        vpiSysFuncSized,
        vpiUserDefn,
        vpiScheduled,
        vpiActive,
        vpiAutomatic,
        vpiCell,
        vpiConfig,
        vpiConstantSelect,
        vpiDecompile,
        vpiDefAttribute,
        vpiDelayType,
        vpiModPathDelay,
        vpiInterModPathDelay,
        vpiMIPDelay,
        vpiIteratorType,
        vpiLibrary,
        vpiOffset,
        vpiResolvedNetType,
        vpiSaveRestartID,
        vpiSaveRestartLocation,
        vpiValid,
        vpiValidFalse,
        vpiValidTrue,
        vpiSigned,
        vpiLocalParam,
        vpiModPathHasIfNone,
        vpiIndexedPartSelectType,
        vpiPosIndexed,
        vpiNegIndexed,
        vpiIsMemory,
        vpiIsProtected,
        vpiStop,
        vpiFinish,
        vpiReset,
        vpiSetInteractiveScope,
        vpiScaledRealTime,
        vpiSimTime,
        vpiSuppressTime,
        vpiSupplyDrive,
        vpiStrongDrive,
        vpiPullDrive,
        vpiWeakDrive,
        vpiLargeCharge,
        vpiMediumCharge,
        vpiSmallCharge,
        vpiHiZ,
        vpiBinStrVal,
        vpiOctStrVal,
        vpiDecStrVal,
        vpiHexStrVal,
        vpiScalarVal,
        vpiIntVal,
        vpiRealVal,
        vpiStringVal,
        vpiVectorVal,
        vpiStrengthVal,
        vpiTimeVal,
        vpiObjTypeVal,
        vpiSuppressVal,
        vpiShortIntVal,
        vpiLongIntVal,
        vpiShortRealVal,
        vpiRawTwoStateVal,
        vpiRawFourStateVal,
        vpiNoDelay,
        vpiInertialDelay,
        vpiTransportDelay,
        vpiPureTransportDelay,
        vpiForceFlag,
        vpiReleaseFlag,
        vpiCancelEvent,
        vpiReturnEvent,
        vpiUserAllocFlag,
        vpiOneValue,
        vpiPropagateOff,
        vpi0,
        vpi1,
        vpiZ,
        vpiX,
        vpiH,
        vpiL,
        vpiDontCare,
        vpiSysTask,
        vpiSysFunc,
        vpiCompile,
        vpiPLI,
        vpiRun,
        vpiNotice,
        vpiWarning,
        vpiError,
        vpiSystem,
        vpiInternal,
        cbValueChange,
        cbStmt,
        cbForce,
        cbRelease,
        cbAtStartOfSimTime,
        cbReadWriteSynch,
        cbReadOnlySynch,
        cbNextSimTime,
        cbAfterDelay,
        cbEndOfCompile,
        cbStartOfSimulation,
        cbEndOfSimulation,
        cbError,
        cbTchkViolation,
        cbStartOfSave,
        cbEndOfSave,
        cbStartOfRestart,
        cbEndOfRestart,
        cbStartOfReset,
        cbEndOfReset,
        cbEnterInteractive,
        cbExitInteractive,
        cbInteractiveScopeChange,
        cbUnresolvedSystf,
        cbAssign,
        cbDeassign,
        cbDisable,
        cbPLIError,
        cbSignal,
        cbNBASynch,
        cbAtEndOfSimTime,
        vpiPackage,
        vpiInterface,
        vpiProgram,
        vpiInterfaceArray,
        vpiProgramArray,
        vpiTypespec,
        vpiModport,
        vpiInterfaceTfDecl,
        vpiRefObj,
        vpiTypeParameter,
        vpiVarBit,
        vpiLongIntVar,
        vpiShortIntVar,
        vpiIntVar,
        vpiShortRealVar,
        vpiByteVar,
        vpiClassVar,
        vpiStringVar,
        vpiEnumVar,
        vpiStructVar,
        vpiUnionVar,
        vpiBitVar,
        vpiLogicVar,
        vpiArrayVar,
        vpiClassObj,
        vpiChandleVar,
        vpiPackedArrayVar,
        vpiVirtualInterfaceVar,
        vpiLongIntTypespec,
        vpiShortRealTypespec,
        vpiByteTypespec,
        vpiShortIntTypespec,
        vpiIntTypespec,
        vpiClassTypespec,
        vpiStringTypespec,
        vpiChandleTypespec,
        vpiEnumTypespec,
        vpiEnumConst,
        vpiIntegerTypespec,
        vpiTimeTypespec,
        vpiRealTypespec,
        vpiStructTypespec,
        vpiUnionTypespec,
        vpiBitTypespec,
        vpiLogicTypespec,
        vpiArrayTypespec,
        vpiVoidTypespec,
        vpiTypespecMember,
        vpiPackedArrayTypespec,
        vpiSequenceTypespec,
        vpiPropertyTypespec,
        vpiEventTypespec,
        vpiClockingBlock,
        vpiClockingIODecl,
        vpiClassDefn,
        vpiConstraint,
        vpiConstraintOrdering,
        vpiDistItem,
        vpiAliasStmt,
        vpiThread,
        vpiMethodFuncCall,
        vpiMethodTaskCall,
        vpiAssert,
        vpiAssume,
        vpiCover,
        vpiRestrict,
        vpiDisableCondition,
        vpiClockingEvent,
        vpiPropertyDecl,
        vpiPropertySpec,
        vpiPropertyExpr,
        vpiMulticlockSequenceExpr,
        vpiClockedSeq,
        vpiClockedProp,
        vpiPropertyInst,
        vpiSequenceDecl,
        vpiCaseProperty,
        vpiCasePropertyItem,
        vpiSequenceInst,
        vpiImmediateAssert,
        vpiImmediateAssume,
        vpiImmediateCover,
        vpiReturn,
        vpiAnyPattern,
        vpiTaggedPattern,
        vpiStructPattern,
        vpiDoWhile,
        vpiOrderedWait,
        vpiWaitFork,
        vpiDisableFork,
        vpiExpectStmt,
        vpiForeachStmt,
        vpiReturnStmt,
        vpiFinal,
        vpiExtends,
        vpiDistribution,
        vpiSeqFormalDecl,
        vpiPropFormalDecl,
        vpiArrayNet,
        vpiEnumNet,
        vpiIntegerNet,
        vpiLogicNet,
        vpiTimeNet,
        vpiStructNet,
        vpiBreak,
        vpiContinue,
        vpiPackedArrayNet,
        vpiConstraintExpr,
        vpiElseConst,
        vpiImplication,
        vpiConstrIf,
        vpiConstrIfElse,
        vpiConstrForeach,
        vpiSoftDisable,
        vpiLetDecl,
        vpiLetExpr,
        vpiActual,
        vpiTypedefAlias,
        vpiIndexTypespec,
        vpiBaseTypespec,
        vpiElemTypespec,
        vpiInputSkew,
        vpiOutputSkew,
        vpiGlobalClocking,
        vpiDefaultClocking,
        vpiDefaultDisableIff,
        vpiOrigin,
        vpiPrefix,
        vpiWith,
        vpiProperty,
        vpiValueRange,
        vpiPattern,
        vpiWeight,
        vpiConstraintItem,
        vpiTypedef,
        vpiImport,
        vpiDerivedClasses,
        vpiInterfaceDecl,
        vpiMethods,
        vpiSolveBefore,
        vpiSolveAfter,
        vpiWaitingProcesses,
        vpiMessages,
        vpiLoopVars,
        vpiConcurrentAssertions,
        vpiMatchItem,
        vpiMember,
        vpiElement,
        vpiAssertion,
        vpiInstance,
        vpiTop,
        vpiUnit,
        vpiJoinType,
        vpiJoin,
        vpiJoinNone,
        vpiJoinAny,
        vpiAccessType,
        vpiForkJoinAcc,
        vpiExternAcc,
        vpiDPIExportAcc,
        vpiDPIImportAcc,
        vpiArrayType,
        vpiStaticArray,
        vpiDynamicArray,
        vpiAssocArray,
        vpiQueueArray,
        vpiArrayMember,
        vpiIsRandomized,
        vpiLocalVarDecls,
        vpiOpStrong,
        vpiRandType,
        vpiNotRand,
        vpiRand,
        vpiRandC,
        vpiPortType,
        vpiInterfacePort,
        vpiModportPort,
        vpiConstantVariable,
        vpiStructUnionMember,
        vpiVisibility,
        vpiPublicVis,
        vpiProtectedVis,
        vpiLocalVis,
        vpiOneStepConst,
        vpiUnboundedConst,
        vpiNullConst,
        vpiAlwaysType,
        vpiAlwaysComb,
        vpiAlwaysFF,
        vpiAlwaysLatch,
        vpiDistType,
        vpiEqualDist,
        vpiDivDist,
        vpiPacked,
        vpiTagged,
        vpiRef,
        vpiVirtual,
        vpiHasActual,
        vpiIsConstraintEnabled,
        vpiSoft,
        vpiClassType,
        vpiMailboxClass,
        vpiSemaphoreClass,
        vpiUserDefinedClass,
        vpiProcessClass,
        vpiMethod,
        vpiIsClockInferred,
        vpiIsDeferred,
        vpiIsFinal,
        vpiIsCoverSequence,
        vpiQualifier,
        vpiNoQualifier,
        vpiUniqueQualifier,
        vpiPriorityQualifier,
        vpiTaggedQualifier,
        vpiRandQualifier,
        vpiInsideQualifier,
        vpiInputEdge,
        vpiOutputEdge,
        vpiGeneric,
        vpiCompatibilityMode,
        vpiMode1364v1995,
        vpiMode1364v2001,
        vpiMode1364v2005,
        vpiMode1800v2005,
        vpiMode1800v2009,
        vpiPackedArrayMember,
        vpiStartLine,
        vpiColumn,
        vpiEndLine,
        vpiEndColumn,
        vpiAllocScheme,
        vpiAutomaticScheme,
        vpiDynamicScheme,
        vpiOtherScheme,
        vpiObjId,
        vpiDPIPure,
        vpiDPIContext,
        vpiDPICStr,
        vpiDPI,
        vpiDPIC,
        vpiDPICIdentifier,
        vpiImplyOp,
        vpiNonOverlapImplyOp,
        vpiOverlapImplyOp,
        vpiAcceptOnOp,
        vpiRejectOnOp,
        vpiSyncAcceptOnOp,
        vpiSyncRejectOnOp,
        vpiOverlapFollowedByOp,
        vpiNonOverlapFollowedByOp,
        vpiNexttimeOp,
        vpiAlwaysOp,
        vpiEventuallyOp,
        vpiUntilOp,
        vpiUntilWithOp,
        vpiUnaryCycleDelayOp,
        vpiCycleDelayOp,
        vpiIntersectOp,
        vpiFirstMatchOp,
        vpiThroughoutOp,
        vpiWithinOp,
        vpiRepeatOp,
        vpiConsecutiveRepeatOp,
        vpiGotoRepeatOp,
        vpiPostIncOp,
        vpiPreIncOp,
        vpiPostDecOp,
        vpiPreDecOp,
        vpiMatchOp,
        vpiCastOp,
        vpiIffOp,
        vpiWildEqOp,
        vpiWildNeqOp,
        vpiStreamLROp,
        vpiStreamRLOp,
        vpiMatchedOp,
        vpiTriggeredOp,
        vpiAssignmentPatternOp,
        vpiMultiAssignmentPatternOp,
        vpiIfOp,
        vpiIfElseOp,
        vpiCompAndOp,
        vpiCompOrOp,
        vpiImpliesOp,
        vpiInsideOp,
        vpiTypeOp,
        vpiAssignmentOp,
        vpiOtherFunc,
        vpiValidUnknown,
        cbStartOfThread,
        cbEndOfThread,
        cbEnterThread,
        cbStartOfFrame,
        cbEndOfFrame,
        cbSizeChange,
        cbCreateObj,
        cbReclaimObj,
        cbEndOfObject,
        vpiCoverageStart,
        vpiCoverageStop,
        vpiCoverageReset,
        vpiCoverageCheck,
        vpiCoverageMerge,
        vpiCoverageSave,
        vpiAssertCoverage,
        vpiFsmStateCoverage,
        vpiStatementCoverage,
        vpiToggleCoverage,
        vpiCovered,
        vpiCoverMax,
        vpiCoveredCount,
        vpiAssertAttemptCovered,
        vpiAssertSuccessCovered,
        vpiAssertFailureCovered,
        vpiAssertVacuousSuccessCovered,
        vpiAssertDisableCovered,
        vpiAssertKillCovered,
        vpiFsmStates,
        vpiFsmStateExpression,
        vpiFsm,
        vpiFsmHandle,
        cbAssertionStart,
        cbAssertionSuccess,
        cbAssertionFailure,
        cbAssertionVacuousSuccess,
        cbAssertionDisabledEvaluation,
        cbAssertionStepSuccess,
        cbAssertionStepFailure,
        cbAssertionLock,
        cbAssertionUnlock,
        cbAssertionDisable,
        cbAssertionEnable,
        cbAssertionReset,
        cbAssertionKill,
        cbAssertionEnablePassAction,
        cbAssertionEnableFailAction,
        cbAssertionDisablePassAction,
        cbAssertionDisableFailAction,
        cbAssertionEnableNonvacuousAction,
        cbAssertionDisableVacuousAction,
        cbAssertionSysInitialized,
        cbAssertionSysOn,
        cbAssertionSysOff,
        cbAssertionSysKill,
        cbAssertionSysLock,
        cbAssertionSysUnlock,
        cbAssertionSysEnd,
        cbAssertionSysReset,
        cbAssertionSysEnablePassAction,
        cbAssertionSysEnableFailAction,
        cbAssertionSysDisablePassAction,
        cbAssertionSysDisableFailAction,
        cbAssertionSysEnableNonvacuousAction,
        cbAssertionSysDisableVacuousAction,
        vpiAssertionLock,
        vpiAssertionUnlock,
        vpiAssertionDisable,
        vpiAssertionEnable,
        vpiAssertionReset,
        vpiAssertionKill,
        vpiAssertionEnableStep,
        vpiAssertionDisableStep,
        vpiAssertionClockSteps,
        vpiAssertionSysLock,
        vpiAssertionSysUnlock,
        vpiAssertionSysOn,
        vpiAssertionSysOff,
        vpiAssertionSysKill,
        vpiAssertionSysEnd,
        vpiAssertionSysReset,
        vpiAssertionDisablePassAction,
        vpiAssertionEnablePassAction,
        vpiAssertionDisableFailAction,
        vpiAssertionEnableFailAction,
        vpiAssertionDisableVacuousAction,
        vpiAssertionEnableNonvacuousAction,
        vpiAssertionSysEnablePassAction,
        vpiAssertionSysEnableFailAction,
        vpiAssertionSysDisablePassAction,
        vpiAssertionSysDisableFailAction,
        vpiAssertionSysEnableNonvacuousAction,
        vpiAssertionSysDisableVacuousAction,
    }
}
