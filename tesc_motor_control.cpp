
test := 0;
MC_Move4(Axis:=MC_Axis004, Execute:=TRUE, Position:=5, Velocity:=7.0, Acceleration:=100, Deceleration:=10, Direction:=positivedir, ErrorID=>MoveDone4, Error=>MoveDone5);
 AryByteTo(KEYENCE_DATA_IN[24],4,_LOW_HIGH,afterConvertData1);
 bAxis4_Move :=TRUE;
 lrAxis4_Speed := 7;
 IF test2 - afterConvertData1 > 0.1 THEN
  //MC_SetPosition4(Axis:=MC_Axis004, Execute:=test6, Position:=0);
  bAxis4_Move :=TRUE;
  lrAxis4_Pos3 := lrAxis4_Pos3 - test4;
  MC_MoveAbsolute4(Axis:=MC_Axis004, Execute:=TRUE, Position:=lrAxis4_Pos3, Velocity:=lrAxis4_Speed, Acceleration:=4000, Deceleration:=20);//, BufferMode:=_mcBlendingNext);
  //MC_MoveRelative4(Axis:=MC_Axis004, Execute:=TRUE, Distance:=lrAxis4_Pos3, Velocity:=lrAxis4_Speed, Acceleration:=4000, Deceleration:=20);//, BufferMode:=_mcBlendingNext);
  //MC_MoveRelative4(Axis:=MC_Axis004, Execute:=TRUE, Distance:= 10.0, Velocity:=7.0, Acceleration:=100, Deceleration:=10, ErrorID=>MoveDone4, Error=>MoveDone5);
  //MC_Move4(Axis:=MC_Axis004, Execute:=TRUE, Position:=5, Velocity:=7.0, Acceleration:=100, Deceleration:=10, Direction:=positivedir, ErrorID=>MoveDone4, Error=>MoveDone5);
  IF lrAxis4_Pos3 < -20 THEN
   lrAxis4_Pos3 := -20;
  END_IF;
 ELSIF test2 - afterConvertData1 < -0.1 THEN
  bAxis4_Move :=TRUE;
  //MC_SetPosition4(Axis:=MC_Axis004, Execute:=test6, Position:=0, ReferenceType:=_mcCommand);
  lrAxis4_Pos2 := lrAxis4_Pos3 + test5;
  MC_MoveAbsolute4(Axis:=MC_Axis004, Execute:=TRUE, Position:=lrAxis4_Pos2, Velocity:=lrAxis4_Speed, Acceleration:=4000, Deceleration:=20);//, BufferMode:=_mcBlendingNext);
  //MC_MoveRelative4(Axis:=MC_Axis004, Execute:=TRUE, Distance:=lrAxis4_Pos2, Velocity:=lrAxis4_Speed, Acceleration:=4000, Deceleration:=20);//, BufferMode:=_mcBlendingNext);
  //MC_Move4(Axis:=MC_Axis004, Execute:=TRUE, Position:=-5, Velocity:=7.0, Acceleration:=100, Deceleration:=10, Direction:=negativedir, ErrorId=>MoveDone4, Error=>MoveDone5);
  IF lrAxis4_Pos2 > 20 THEN
   lrAxis4_Pos2 := 20;
  END_IF;
 END_IF;
 
 

 

IF Enable = TRUE THEN
 
 unStep := UINT#0;
 glrHeight3_Manual_Offset := 0;
 glrLVS_Manual_Offset := 0;
 lrAxis1_Pos1 := MC_Axis001.Act.Pos;
 PrelrAxis1_Speed := 0;
 dnEdge := 0;
 Output := 0;
 lrError_R := 0;
 rSmoothCnt1 := grSlopeTime[gnPosition, 1] / 20;
 rSmoothCnt2 := 0;
 rSmoothCnt3 := 0;
 rSmoothCnt4 := 0;
 rSmoothCnt5 := 0;
 rSmoothCnt6 := 0;
 
 rmSmoothCnt1 := 0;
 rmSmoothCnt2 := 0;
 rmSmoothCnt3 := 0;
 rmSmoothCnt4 := 0;
 rmSmoothCnt5 := 0;
 rmSmoothCnt6 := 0;
 
 rmSmoothCntR := 0;
END_IF;

lrError_R := (DINT_TO_LREAL(In:=gdnCalAngle) / REAL#1000.0) * REAL#1.0;// + lrPreError_R * 0.35;
lrError_Z1 := (DINT_TO_LREAL(In:=(gdnHeight2)) / REAL#1000.0 * (-1)) * REAL#0.9 + lrPreError_Z1 * REAL#0.1 ;
lrError_Z2 := ((DINT_TO_LREAL(In:=(gdnHeight3 - gdnHeight3_Offset)) / REAL#1000.0) + glrHeight3_Manual_Offset + lrHeight3Offset3) * 0.8 + lrPreError_Z2 * 0.2 ;

IF lrError_R > 25 THEN
 lrError_R := 25;
ELSIF lrError_R < -20 THEN
 lrError_R := -20;
END_IF;
R_TRIG1(Clk:=Pen_Y_P, Q=>Pen_Y_P_R);
R_TRIG2(Clk:=Pen_Y_N, Q=>Pen_Y_N_R);
IF Pen_Y_P_R = TRUE THEN
 glrLVS_Manual_Offset := glrLVS_Manual_Offset + 0.15;
ELSIF Pen_Y_N_R = TRUE THEN
 glrLVS_Manual_Offset := glrLVS_Manual_Offset - 0.15;
END_IF;

R_TRIG3(Clk:=Pen_Z2_P, Q=>Pen_Z2_P_R);
R_TRIG4(Clk:=Pen_Z2_N, Q=>Pen_Z2_N_R);
IF Pen_Z2_P_R = TRUE THEN
 glrHeight3_Manual_Offset := glrHeight3_Manual_Offset + 0.15;
ELSIF Pen_Z2_N_R = TRUE THEN
 glrHeight3_Manual_Offset := glrHeight3_Manual_Offset - 0.15;
END_IF;

IF N4_Output_Bit_20 = TRUE THEN
 bRefresh :=  TRUE;
END_IF;

//TOF(In:=var0, PT:=var1, Q=>var2, ET=>var3);
//IF gbTest = TRUE THEN
IF gbLVSUpdateOK = TRUE THEN
 IF(dnEdge <> gdnEdgeAve) AND (bRefresh =  TRUE) THEN
  dnEdge := gdnEdgeAve;
  bRefresh :=  FALSE;
  //lrTarget_Y_Pos := MC_Axis004.Act.Pos + glrLVS_Manual_Offset + lrRangeY_Offset3 + (DINT_TO_LREAL(In:=(LVS_Offset - dnEdge)) / REAL#1000.0);
  lrTarget_Y_Pos := MC_Axis004.Act.Pos + glrLVS_Manual_Offset + (DINT_TO_LREAL(In:=(LVS_Offset - dnEdge)) / REAL#1000.0);
  lrError_Y := (lrTarget_Y_Pos - MC_Axis004.Act.Pos); //* 0.8 + lrPreError_Y * 0.2;
  IF ABS(lrError_Y) > 1.2 THEN
   lrError_Y := 0;
   lrTarget_Y_Pos := MC_Axis004.Act.Pos;
  ELSIF (lrError_Y > 0.3) THEN
   lrError_Y := 0.3;
   lrTarget_Y_Pos := MC_Axis004.Act.Pos + lrError_Y;
  ELSIF (lrError_Y < -0.3) THEN
   lrError_Y := -0.3;
   lrTarget_Y_Pos := MC_Axis004.Act.Pos + lrError_Y;
  END_IF;
 END_IF;
ELSE
 dnEdge := gdnEdgeAve;
 lrTarget_Y_Pos := MC_Axis004.Act.Pos;
END_IF;
//ELSE
// lrTarget_Y_Pos := MC_Axis004.Act.Pos;
//END_IF;

lrError_Y := (lrTarget_Y_Pos - MC_Axis004.Act.Pos); //* 0.8 + lrPreError_Y * 0.2;

IF gunRange = 0 THEN
 rPgain_Z1 := 0.5;
 rDgain_Z1 := 0.01;
 rPgain_Z2 := 0.09;
 rDgain_Z2 := 0.1;
 rPgain_R := 0.02;
 rDgain_R := 0.1;
 rPgain_Y := 0.01;
 rDgain_Y := 0.01;
 dnHeight3Offset := 0;
 lrRangeY_Offset3 := 0;
 glrLVS_Manual_Offset := 0;
 glrHeight3_Manual_Offset := 0;
ELSE
 rPgain_Z1 := grPgain_Z1[gunRange]  / 1000;
 rDgain_Z1 := grDgain_Z1[gunRange]  / 1000;
 rPgain_Z2 := grPgain_Z2[gunRange]  / 1000;
 rDgain_Z2 := grDgain_Z1[gunRange]  / 1000;
 rPgain_R := grPgain_R[gunRange]  / 1000;
 rDgain_R := grDgain_R[gunRange]  / 1000;
 rPgain_Y := grPgain_Y[gunRange]  / 1000;
 rDgain_Y := grDgain_Y[gunRange]  / 1000;
END_IF;

CASE gunRange OF
 0:
  bAxis1_Move := FALSE;
  bAxis2_Move := FALSE;
  bAxis3_Move := FALSE;
  bAxis4_Move := FALSE;
  bAxis5_Move := FALSE;
  Output := 0;
  rPgain_Z1 := 0;
  rDgain_Z1 := 0.01;
  rPgain_Z2 := 0.09;
  rDgain_Z2 := 0.1;
  rPgain_R := 0;
  rDgain_R := 0.1;
  rPgain_Y := 0.01;
  rDgain_Y := 0.01;
  bInitialErrorR := FALSE;
 1:
  Output := 1;
  lrHeight3Offset := glrZ2Offset[gnPosition,1] / 1000;
  lrRangeY_Offset := glrYOffset[gnPosition,1] / 1000;
  //IF  MC_Axis003.Act.Pos > -2.5 THEN
  // N4_Output_Bit_21 := TRUE;
  // N4_Output_Bit_22 := FALSE;
  //END_IF;
  //lrSmoothNumY := 50;
  MC_Move4(Axis:=MC_Axis004, Execute:=TRUE, Position:=5, Velocity:=7.0, Acceleration:=100, Deceleration:=10, Direction:=positivedir, ErrorID=>MoveDone4, Error=>MoveDone5);
  rSmoothCnt2 := 0;
  rSmoothCnt3 := 0;
  rSmoothCnt4 := 0;
  rSmoothCnt5 := 0;
  rSmoothCnt6 := 0;
  IF(rSmoothCnt1 < grSlopeTime[gnPosition, 1] / 20) THEN
   rSmoothCnt1 := rSmoothCnt1 + 1;
   lrWeldSpeed := lrWeldSpeed + ((grSpeed[gnPosition, 6] - grSpeed[gnPosition, 1]) / grSlopeTime[gnPosition, 1] / 20);
  ELSE
   lrWeldSpeed := grSpeed[gnPosition, 1];
  END_IF;
  rPgain_Z1 := 0;
  rPgain_R := 0;
  
  rmSmoothCnt2 := 0;
  rmSmoothCnt3 := 0;
  rmSmoothCnt4 := 0;
  rmSmoothCnt5 := 0;
  rmSmoothCnt6 := 0;
  
  bInitialErrorR := FALSE;
  //IF(gbCorr_Select = TRUE) THEN
  // bInitialErrorR := FALSE;
  //ELSE
  // bInitialErrorR := TRUE;
  //END_IF;
 2:
  lrHeight3Offset := glrZ2Offset[gnPosition,2] / 1000;
  lrRangeY_Offset := glrYOffset[gnPosition,2] / 1000;
  rPgain_Z1 := 0.2;
  //N4_Output_Bit_21 := TRUE;
  //N4_Output_Bit_22 := FALSE;
  //lrSmoothNumY := 50;
  rSmoothCnt1 := 0;
  rSmoothCnt3 := 0;
  rSmoothCnt4 := 0;
  rSmoothCnt5 := 0;
  rSmoothCnt6 := 0;
  IF(rSmoothCnt2 < grSlopeTime[gnPosition, 2] / 20) THEN
   rSmoothCnt2 := rSmoothCnt2 + 1;
   lrWeldSpeed := lrWeldSpeed + (grSpeed[gnPosition, 2] - grSpeed[gnPosition, 1]) / (grSlopeTime[gnPosition, 2] / 20);
  ELSE
   lrWeldSpeed := grSpeed[gnPosition, 2];
   IF (MC_Axis003.Act.Pos < -50) THEN
    lrWeldSpeed :=  grSpeed[gnPosition, 2] * 0.95;
   ELSIF (MC_Axis003.Act.Pos < -3) THEN
    lrWeldSpeed :=  grSpeed[gnPosition, 2] * 0.75;
   ELSE
    lrWeldSpeed :=  grSpeed[gnPosition, 2] * 0.95;
   END_IF;
  END_IF;

  rmSmoothCnt1 := 0;
  rmSmoothCnt3 := 0;
  rmSmoothCnt4 := 0;
  rmSmoothCnt5 := 0;
  rmSmoothCnt6 := 0;
  (*
  IF(rmSmoothCnt2 < 20) THEN
   rmSmoothCnt2 := rmSmoothCnt2 + 1;
   lrError_R := 0;
  ELS
  
  *)
  IF(rmSmoothCnt2 < 50) AND bInitialErrorR = TRUE THEN
   rmSmoothCnt2 := rmSmoothCnt2 + 1;
   lrError_R := lrError_R * ((rmSmoothCnt2)/50);
   lrError_Z1 := lrError_Z1 * ((rmSmoothCnt2)/50);
   rPgain_Z1 := rPgain_Z1 + ((grPgain_Z1[gunRange] - 0) / 1000) / (50);
   rPgain_R := rPgain_R + ((grPgain_R[gunRange] - 0) / 1000) / (50);
  ELSE
   rPgain_Z1 := grPgain_Z1[gunRange]  / 1000;
   rPgain_R := grPgain_R[gunRange]  / 1000;
  END_IF;

  IF lrError_R < -0.02 THEN
   bInitialErrorR := TRUE;
  END_IF;
  
 3:
  lrHeight3Offset := glrZ2Offset[gnPosition,3] / 1000;
  lrRangeY_Offset := glrYOffset[gnPosition,3] / 1000;
 // N4_Output_Bit_21 := TRUE;
  //N4_Output_Bit_22 := FALSE;

  rSmoothCnt1 := 0;
  rSmoothCnt2 := 0;
  rSmoothCnt4 := 0;
  rSmoothCnt5 := 0;
  rSmoothCnt6 := 0;
  
  IF(rSmoothCnt3 < grSlopeTime[gnPosition, 3] / 20) THEN
   rSmoothCnt3 := rSmoothCnt3 + 1;
   lrWeldSpeed := lrWeldSpeed + (grSpeed[gnPosition, 3] - grSpeed[gnPosition, 2]) / (grSlopeTime[gnPosition, 3] / 20);
   rPgain_Z1 := rPgain_Z1 + ((grPgain_Z1[gunRange] - grPgain_Z1[2]) / 1000) / (grSlopeTime[gnPosition, 3] / 20);
   rPgain_R := rPgain_R + ((grPgain_R[gunRange] - grPgain_R[2]) / 1000) / (grSlopeTime[gnPosition, 3] / 20);
  ELSE
   lrWeldSpeed := grSpeed[gnPosition, 3];
   rPgain_Z1 := grPgain_Z1[gunRange]  / 1000;
   rPgain_R := grPgain_R[gunRange]  / 1000;
  END_IF;
  
  rmSmoothCnt1 := 0;
  rmSmoothCnt2 := 0;
  rmSmoothCnt4 := 0;
  rmSmoothCnt5 := 0;
  rmSmoothCnt6 := 0;
  
 
 4:
  lrHeight3Offset := glrZ2Offset[gnPosition,4] / 1000;
  lrRangeY_Offset := glrYOffset[gnPosition,4] / 1000;
  //IF  MC_Axis004.Act.Pos <5 THEN
  // N4_Output_Bit_21 := TRUE;
  // N4_Output_Bit_22 := FALSE;
  //END_IF;

  rSmoothCnt1 := 0;
  rSmoothCnt2 := 0;
  rSmoothCnt3 := 0;
  rSmoothCnt5 := 0;
  rSmoothCnt6 := 0;
  IF(rSmoothCnt4 < grSlopeTime[gnPosition, 4] / 20) THEN
   rSmoothCnt4 := rSmoothCnt4 + 1;
   lrWeldSpeed := lrWeldSpeed + (grSpeed[gnPosition, 4] - grSpeed[gnPosition, 3]) / (grSlopeTime[gnPosition, 4] / 20);
   rPgain_Z1 := rPgain_Z1 + ((grPgain_Z1[gunRange] - grPgain_Z1[3]) / 1000) / (grSlopeTime[gnPosition, 4] / 20);
   rPgain_R := rPgain_R + ((grPgain_R[gunRange] - grPgain_R[3]) / 1000) / (grSlopeTime[gnPosition, 4] / 20);
  ELSE
   lrWeldSpeed := grSpeed[gnPosition, 4];
   rPgain_Z1 := grPgain_Z1[gunRange]  / 1000;
   rPgain_R := grPgain_R[gunRange]  / 1000;
  END_IF;
  
  rmSmoothCnt1 := 0;
  rmSmoothCnt2 := 0;
  rmSmoothCnt3 := 0;
  rmSmoothCnt5 := 0;
  rmSmoothCnt6 := 0;
  
  IF(rmSmoothCnt4 < 100) THEN
   rmSmoothCnt4 := rmSmoothCnt4 + 1;
   lrError_R := lrError_R + LREAL#5.0 * (rmSmoothCnt4 /100);
  ELSE
   rmSmoothCnt4 := 200;
   lrError_R := lrError_R + LREAL#5.0;
  END_IF;
  
 5:
  lrHeight3Offset := glrZ2Offset[gnPosition,5] / 1000;
  lrRangeY_Offset := glrYOffset[gnPosition,5] / 1000;
 // N4_Output_Bit_21 := TRUE;
 // N4_Output_Bit_22 := FALSE;

  rSmoothCnt1 := 0;
  rSmoothCnt2 := 0;
  rSmoothCnt3 := 0;
  rSmoothCnt4 := 0;
  rSmoothCnt6 := 0;
  IF(rSmoothCnt5 < grSlopeTime[gnPosition, 5] / 20) THEN
   rSmoothCnt5 := rSmoothCnt5 + 1;
   lrWeldSpeed := lrWeldSpeed + (grSpeed[gnPosition, 5] - grSpeed[gnPosition, 4]) / (grSlopeTime[gnPosition, 5] / 20);
   rPgain_Z1 := rPgain_Z1 + ((grPgain_Z1[gunRange] - grPgain_Z1[4]) / 1000) / (grSlopeTime[gnPosition, 5] / 20);
   rPgain_R := rPgain_R + ((grPgain_R[gunRange] - grPgain_R[4]) / 1000) / (grSlopeTime[gnPosition, 5] / 20);
  ELSE
   lrWeldSpeed := grSpeed[gnPosition, 5];
   rPgain_Z1 := grPgain_Z1[gunRange]  / 1000;
   rPgain_R := grPgain_R[gunRange]  / 1000;
  END_IF;
  
  rmSmoothCnt1 := 0;
  rmSmoothCnt2 := 0;
  rmSmoothCnt3 := 0;
  //rmSmoothCnt4 := 0;
  rmSmoothCnt6 := 0;
  
  IF(rmSmoothCnt4 > 0) THEN
   rmSmoothCnt4 := rmSmoothCnt4 - 1;
   lrError_R := lrError_R + LREAL#5.0 * (rmSmoothCnt4 /200);
  END_IF;
   
 6:
  lrHeight3Offset := glrZ2Offset[gnPosition,6] / 1000;
  lrRangeY_Offset := glrYOffset[gnPosition,6] / 1000;
 // N4_Output_Bit_21 := FALSE;
 // N4_Output_Bit_22 := FALSE;

  rSmoothCnt1 := 0;
  rSmoothCnt2 := 0;
  rSmoothCnt3 := 0;
  rSmoothCnt4 := 0;
  rSmoothCnt5 := 0;
  
  IF(rSmoothCnt6 < grSlopeTime[gnPosition, 6] / 20) THEN
   rSmoothCnt6 := rSmoothCnt6 + 1;
   lrWeldSpeed := lrWeldSpeed + (grSpeed[gnPosition, 6] - grSpeed[gnPosition, 5]) / (grSlopeTime[gnPosition, 6] / 20);
   rPgain_Z1 := rPgain_Z1 + ((grPgain_Z1[gunRange] - grPgain_Z1[5]) / 1000) / (grSlopeTime[gnPosition, 6] / 20);
   rPgain_R := rPgain_R + ((grPgain_R[gunRange] - grPgain_R[5]) / 1000) / (grSlopeTime[gnPosition, 6] / 20);
  ELSE
   lrWeldSpeed := grSpeed[gnPosition, 6];
   rPgain_Z1 := grPgain_Z1[gunRange]  / 1000;
   rPgain_R := grPgain_R[gunRange]  / 1000;
   IF (MC_Axis003.Act.Pos > 45) THEN
    lrWeldSpeed :=  grSpeed[gnPosition, 6] * 0.95;
   ELSIF (MC_Axis003.Act.Pos > 10) THEN
    lrWeldSpeed :=  grSpeed[gnPosition, 6] * 0.8;
   ELSE
    lrWeldSpeed :=  grSpeed[gnPosition, 6] * 0.95;
   END_IF;
  END_IF;
  
  rmSmoothCnt1 := 0;
  rmSmoothCnt2 := 0;
  rmSmoothCnt3 := 0;
  rmSmoothCnt4 := 0;
  rmSmoothCnt5 := 0;

 END_CASE;
 
 
lrMotionSpeed := lrWeldSpeed / LREAL#60;

IF gunRange = 1 THEN
 lrAxis1_Speed := lrMotionSpeed;

 IF(ABS(In:=gdnHeight1) < 200) THEN
  lrAxis2_Dis := 0;
 ELSE
  lrAxis2_Dis := DINT_TO_LREAL(In:=gdnHeight2) / 1000 * (-1);
 END_IF;
 
 lrAxis2_Speed := lrAxis2_Dis;
 
 IF(rmSmoothCnt1 < 50) THEN
  rmSmoothCnt1 := rmSmoothCnt1 + 1;
  lrAxis3_Dis := (-MC_Axis003.Act.Pos);// * (rmSmoothCnt1 / 50);
 ELSE
  IF(ABS(In:=MC_Axis003.Act.Pos) < 0.2) THEN
   lrAxis3_Dis := 0;
  ELSE
   lrAxis3_Dis := (-MC_Axis003.Act.Pos);
  END_IF;
 END_IF;

 lrAxis3_Speed := lrAxis3_Dis * 0.5;
 
ELSE //// Range = 1  외

 lrCal_R := MC_Axis003.Act.Pos + lrError_R;
 
 //lrAxis1_Speed := (lrMotionSpeed - lrAxis2_Speed_Temp) * COS(In:=DegToRad(In:=lrCal_R));
 lrAxis2_Dis :=  (lrError_Z1 * rPgain_Z1 + (lrError_Z1 - lrPreError_Z1) * rDgain_Z1) ;
 lrAxis1_Speed := lrMotionSpeed * COS(In:=DegToRad(In:=lrCal_R)) - (lrAxis2_Dis / 0.02) * SIN(In:=DegToRad(In:=lrCal_R));
 
 IF lrAxis1_Speed < 0 THEN
  lrAxis1_Speed := 0;
 END_IF;
 
 lrAxis2_Dis := lrMotionSpeed * SIN(In:=DegToRad(In:=lrCal_R)) * 0.02 + lrAxis2_Dis * COS(In:=DegToRad(In:=lrCal_R));
 lrAxis2_Speed := lrAxis2_Dis / 0.02;
 lrAxis2_Dis := lrAxis2_Dis * 2;
 lrAxis2_Pos := MC_Axis002.Act.Pos + lrAxis2_Dis;

 lrAxis3_Dis := (lrError_R * rPgain_R  + (lrError_R - lrPreError_R) * rDgain_R );
 lrAxis3_Speed := lrAxis3_Dis / 0.02;
 lrAxis3_Dis := lrAxis3_Dis * 2;
 lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
 
 IF gunRange = 2 THEN
  IF bInitialErrorR = TRUE THEN
   lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
  ELSE
   lrAxis3_Pos := MC_Axis003.Act.Pos;
   lrAxis3_Speed := 0;
  END_IF;
 ELSE
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
 END_IF;

END_IF;

PrelrAxis3_Speed := lrAxis3_Speed;


 
IF PreLVS_Offset <> glrLVS_Manual_Offset THEN
 lrAxis4_Dis := glrLVS_Manual_Offset - PreLVS_Offset;
 PreLVS_Offset := glrLVS_Manual_Offset;
 lrAxis4_Speed := 8;
 bOnePeriod := TRUE;
ELSE
 IF bOnePeriod = FALSE THEN
  IF gbTest = TRUE THEN
   lrAxis4_Dis := lrError_Y * rPgain_Y; // + (lrError_Y - lrPreError_Y) * rDgain_Y) ;
   lrAxis4_Speed := lrAxis4_Dis / 0.02;
  ELSE
   lrAxis4_Dis := 0;
  END_IF;
 ELSE
  lrAxis4_Dis := 0;
  lrAxis4_Speed := 0;
  bOnePeriod := FALSE;
 END_IF;
END_IF;


//IF gunRange = 2 OR gunRange = 4 OR gunRange = 6 THEN
 IF PrelrRangeY_Offset <> lrRangeY_Offset THEN
  IF rmSmoothCntY < 100 THEN
   rmSmoothCntY := rmSmoothCntY + 1;
  END_IF;
  IF rmSmoothCntY = 1 THEN
   lrRangeY_Offset2 := lrRangeY_Offset - lrRangeYCurOffset;
  ELSIF rmSmoothCntY = 100 THEN
   PrelrRangeY_Offset := lrRangeY_Offset;
   lrRangeYCurOffset := lrRangeY_Offset;
   lrRangeY_Offset3 := lrRangeY_Offset;
  END_IF;
  lrRangeY_Offset3 := lrRangeY_Offset3 + lrRangeY_Offset2 / 100;
  lrAxis4_Dis := lrAxis4_Dis + (lrRangeY_Offset2) / 100 ;
  lrAxis4_Speed := lrAxis4_Dis / 0.02;
 ELSE
  rmSmoothCntY := 0;
 END_IF;
//END_IF;
//IF gunRange = 2 OR gunRange = 4 OR gunRange = 6 THEN
// IF PrelrRange_Offset <> lrRange_Offset THEN
//  IF rmSmoothCntY < 200 THEN
//   rmSmoothCntY := rmSmoothCntY + 1;
//  END_IF;
//  IF rmSmoothCntY = 1 THEN
//   lrRange_Offset2 := lrRange_Offset - lrRange_Offset2;
//  ELSIF rmSmoothCntY = 200 THEN
//   PrelrRange_Offset := lrRange_Offset;
//   lrRange_Offset2 := lrRange_Offset;
//  END_IF;
//  lrAxis4_Dis := lrAxis4_Dis + (lrRange_Offset2) / 200 ;
//  lrAxis4_Speed := lrAxis4_Dis / 0.02;
// ELSE
//  rmSmoothCntY := 0;
// END_IF;
//END_IF;

 

IF PreHeight3Offset <> lrHeight3Offset THEN
 IF rmSmoothCntR < 50 THEN
  rmSmoothCntR := rmSmoothCntR + 1;
 END_IF;
 IF rmSmoothCntR = 1 THEN
  lrHeight3Offset2 := lrHeight3Offset - lrHeight3CurOffset;
 ELSIF rmSmoothCntR = 50 THEN
  PreHeight3Offset := lrHeight3Offset;
  lrHeight3CurOffset := lrHeight3Offset;
  lrHeight3Offset3 := lrHeight3Offset;
 END_IF;
 lrHeight3Offset3 := lrHeight3Offset3 + (lrHeight3Offset2) / 50;
 lrAxis5_Dis := lrAxis5_Dis + (lrHeight3Offset2) / 50 ;
 lrAxis5_Speed:= lrAxis5_Dis / 0.02;
ELSE
 rmSmoothCntR := 0;
END_IF;

IF PreHeight3_Manual_Offset <> glrHeight3_Manual_Offset THEN
 lrAxis5_Dis := glrHeight3_Manual_Offset - PreHeight3_Manual_Offset;
 PreHeight3_Manual_Offset := glrHeight3_Manual_Offset;
 lrAxis5_Speed := 3;
 bOnePeriod2 := TRUE;
ELSE
 IF bOnePeriod2 = FALSE THEN
  lrAxis5_Dis :=  (lrError_Z2 * rPgain_Z2 + (lrError_Z2 - lrPreError_Z2) * rDgain_Z2) ;
  lrAxis5_Speed:= lrAxis5_Dis / 0.02;
  lrAxis5_Dis := lrAxis5_Dis *2;
 ELSE
  lrAxis5_Dis := 0;
  lrAxis5_Speed := 0;
  bOnePeriod2 := FALSE;
 END_IF;
END_IF;

 

lrAxis2_Pos := MC_Axis002.Act.Pos + lrAxis2_Dis;
lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
lrAxis4_Pos := MC_Axis004.Act.Pos + lrAxis4_Dis;
lrAxis5_Pos := MC_Axis005.Act.Pos + lrAxis5_Dis;

 


IF (gunRange = 2) THEN
 IF lrAxis3_Dis > 0 THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
  lrAxis3_Speed := 0;
 END_IF;
ELSIF (gunRange = 5) THEN
 IF lrAxis3_Dis < 0 THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
  lrAxis3_Speed := 0;
 END_IF;
ELSIF (gunRange = 6) THEN
 IF lrAxis3_Dis > 0 THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
  lrAxis3_Speed := 0;
 END_IF;
 (*
 IF (MC_Axis003.Act.Pos < 5.0) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos < 7.0) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Dis := lrAxis3_Dis * (MC_Axis003.Act.Pos - 5.0) / 2.0;
  lrAxis3_Speed := lrAxis3_Dis / 0.03;
  lrAxis3_Pos := MC_Axis003.Act.Pos +lrAxis3_Dis;
 END_IF;
 *)
END_IF;


IF(gbCorr_Select = TRUE) THEN //Large
 IF(MC_Axis003.Act.Pos < -50) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos < -47.5) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Dis := lrAxis3_Dis * (50 + MC_Axis003.Act.Pos) / 7.5;
  lrAxis3_Speed := lrAxis3_Dis / 0.03;
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
 ELSIF (MC_Axis003.Act.Pos > 60) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos > 57.5) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Dis := lrAxis3_Dis * (60 - MC_Axis003.Act.Pos) / 7.5;
  lrAxis3_Speed := lrAxis3_Dis / 0.03;
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
 END_IF;
ELSE
 IF(MC_Axis003.Act.Pos < -50) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos < -47.5) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Dis := lrAxis3_Dis * (50 + MC_Axis003.Act.Pos) / 7.5;
  lrAxis3_Speed := lrAxis3_Dis / 0.03;
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
 ELSIF (MC_Axis003.Act.Pos > 60) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos > 57.5) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Dis := lrAxis3_Dis * (60 - MC_Axis003.Act.Pos) / 7.5;
  lrAxis3_Speed := lrAxis3_Dis / 0.03;
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
 END_IF;
 (*
 IF(MC_Axis002.Act.Pos > -5) THEN
  IF (MC_Axis003.Act.Pos < -55) AND (lrAxis3_Dis < 0) THEN
   lrAxis3_Pos := MC_Axis003.Act.Pos;
  ELSIF (MC_Axis003.Act.Pos < -47.5) AND (lrAxis3_Dis < 0) THEN
   lrAxis3_Dis := lrAxis3_Dis * (55 + MC_Axis003.Act.Pos) / 7.5;
   lrAxis3_Speed := lrAxis3_Dis / 0.03;
   lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
  END_IF;
 ELSE
  IF(MC_Axis003.Act.Pos > 60) AND (lrAxis3_Dis > 0) THEN
   lrAxis3_Pos := MC_Axis003.Act.Pos;
  ELSIF (MC_Axis003.Act.Pos > 47.5) AND (lrAxis3_Dis > 0) THEN
   lrAxis3_Dis := lrAxis3_Dis * (60 - MC_Axis003.Act.Pos) / 7.5;
   lrAxis3_Speed := lrAxis3_Dis / 0.03;
   lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis;
  END_IF;
 
 END_IF;
 
 IF (MC_Axis003.Act.Pos > 65) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos > 57.5) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Dis := lrAxis3_Dis * (65 - MC_Axis003.Act.Pos) / 7.5;
  lrAxis3_Speed := lrAxis3_Dis / 0.03;
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis ;
 END_IF;
  *)
END_IF;


lrAxis1_Speed := ABS(In:=lrAxis1_Speed);
lrAxis2_Speed := ABS(In:=lrAxis2_Speed);
lrAxis3_Speed := ABS(In:=lrAxis3_Speed);
lrAxis4_Speed := ABS(In:=lrAxis4_Speed);
lrAxis5_Speed := ABS(In:=lrAxis5_Speed);

IF(lrAxis1_Speed > 7.5) THEN
 lrAxis1_Speed := 7.5;
END_IF;

IF(lrAxis2_Speed > 6) THEN
 lrAxis2_Speed := 6;
END_IF;

IF(lrAxis3_Speed > 70) THEN
 lrAxis3_Speed := 70;
END_IF;

IF(lrAxis4_Speed > 8) THEN
 lrAxis4_Speed := 8;
END_IF;

IF(lrAxis5_Speed > 3) THEN
 lrAxis5_Speed := 3;
END_IF;

 


(*
IF(gbCorr_Select = TRUE) THEN //Large
 IF(MC_Axis003.Act.Pos > 55) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos > 47.5) AND (lrAxis3_Dis > 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis * (55 - MC_Axis003.Act.Pos) / 7.5 ;
 ELSIF (MC_Axis003.Act.Pos < -65) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos < -57.5) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis * (65 + MC_Axis003.Act.Pos) / 7.5 ;
 END_IF;
ELSE
 IF(MC_Axis002.Act.Pos > -5) THEN
  IF (MC_Axis003.Act.Pos > 55) AND (lrAxis3_Dis > 0) THEN
   lrAxis3_Pos := MC_Axis003.Act.Pos;
  ELSIF (MC_Axis003.Act.Pos > 47.5) AND (lrAxis3_Dis > 0) THEN
   lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis * (55 - MC_Axis003.Act.Pos) / 7.5 ;
  END_IF;
 ELSE
  IF(MC_Axis003.Act.Pos > 60) AND (lrAxis3_Dis > 0) THEN
   lrAxis3_Pos := MC_Axis003.Act.Pos;
  ELSIF (MC_Axis003.Act.Pos > 47.5) AND (lrAxis3_Dis > 0) THEN
   lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis * (60 - MC_Axis003.Act.Pos) / 7.5 ;
  END_IF;
 END_IF;
 
 IF (MC_Axis003.Act.Pos < -65) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos;
 ELSIF (MC_Axis003.Act.Pos < -57.5) AND (lrAxis3_Dis < 0) THEN
  lrAxis3_Pos := MC_Axis003.Act.Pos + lrAxis3_Dis * (65 + MC_Axis003.Act.Pos) / 7.5 ;
 END_IF;
END_IF;
*)

(*
IF(lrAxis1_Speed < 0) THEN
 lrAxis1_Speed := lrAxis1_Speed  * (-1);
 lrAxis1_Direction :=_eMC_DIRECTION#_mcNegativeDirection;
ELSE
 lrAxis1_Direction :=_eMC_DIRECTION#_mcPositiveDirection;
END_IF;
IF(lrAxis2_Speed < 0) THEN
 lrAxis2_Speed := lrAxis2_Speed  * (-1);
 lrAxis2_Direction :=_eMC_DIRECTION#_mcNegativeDirection;
END_IF;
IF(lrAxis3_Speed < 0) THEN
 lrAxis3_Speed := lrAxis3_Speed  * (-1);
 lrAxis3_Direction :=_eMC_DIRECTION#_mcNegativeDirection;
END_IF;
IF(lrAxis4_Speed < 0) THEN
 lrAxis4_Speed := lrAxis4_Speed  * (-1);
 lrAxis4_Direction :=_eMC_DIRECTION#_mcNegativeDirection;
END_IF;
IF(lrAxis5_Speed < 0) THEN
 lrAxis5_Speed := lrAxis5_Speed  * (-1);
 lrAxis5_Direction :=_eMC_DIRECTION#_mcNegativeDirection;
END_IF;
*)

IF (N3_Input_Bit_01 = TRUE) OR (N3_Input_Bit_02 = TRUE) OR (N3_Input_Bit_03 = TRUE) OR (N3_Input_Bit_04 = TRUE) THEN
 Output := -1;
END_IF;

bPeriod := bPeriod XOR 1;

//IF bPeriod = FALSE THEN
 IF (gunRange >= 1) AND (Output <> -1) THEN
  IF(PrelrAxis1_Speed <> lrAxis1_Speed) THEN
   bAxis1_Move := TRUE;
   PrelrAxis1_Speed := lrAxis1_Speed;
  END_IF;
  IF(lrAxis2_Speed <> 0) THEN
   bAxis2_Move := TRUE;
  END_IF;
  IF(lrAxis3_Speed <> 0) THEN
   bAxis3_Move := TRUE;
  END_IF;
  IF(lrAxis4_Speed <> 0) THEN
   bAxis4_Move := TRUE;
  END_IF;
  IF(lrAxis5_Speed <> 0) THEN
   bAxis5_Move := TRUE;
  END_IF;
 END_IF;
 //MC_MoveAbsolute1(Axis:=MC_Axis001, Execute:=bAxis1_Move, Position:=lrAxis1_Dis, Velocity:=lrAxis1_Speed, Acceleration:=800, Deceleration:=800);
 MC_MoveVelocity1(Axis:=MC_Axis001, Execute:=bAxis1_Move, Velocity:=lrAxis1_Speed, Acceleration:=800, Deceleration:=10, Direction:= lrAxis1_Direction);
 MC_MoveAbsolute2(Axis:=MC_Axis002, Execute:=bAxis2_Move, Position:=lrAxis2_Pos, Velocity:=lrAxis2_Speed, Acceleration:=5000, Deceleration:=5, Jerk:=300000);
 MC_MoveAbsolute3(Axis:=MC_Axis003, Execute:=bAxis3_Move, Position:=lrAxis3_Pos, Velocity:=lrAxis3_Speed, Acceleration:=3000, Deceleration:=24); //, BufferMode:=_mcBlendingNext);
 MC_MoveAbsolute4(Axis:=MC_Axis004, Execute:=bAxis4_Move, Position:=lrAxis4_Pos, Velocity:=lrAxis4_Speed, Acceleration:=4000, Deceleration:=20); //, BufferMode:=_mcBlendingNext);
 MC_MoveAbsolute5(Axis:=MC_Axis005, Execute:=bAxis5_Move, Position:=lrAxis5_Pos, Velocity:=lrAxis5_Speed, Acceleration:=2000, Deceleration:=10); //, BufferMode:=_mcBlendingNext);
//ELSE
 bAxis1_Move := FALSE;
 bAxis2_Move := FALSE;
 bAxis3_Move := FALSE;
 bAxis4_Move := FALSE;
 bAxis5_Move := FALSE;
 //MC_MoveAbsolute1(Axis:=MC_Axis001, Execute:=bAxis1_Move, Position:=lrAxis1_Dis, Velocity:=lrAxis1_Speed, Acceleration:=800, Deceleration:=800);
 MC_MoveVelocity1(Axis:=MC_Axis001, Execute:=bAxis1_Move, Velocity:=lrAxis1_Speed, Acceleration:=800, Deceleration:=10, Direction:= lrAxis1_Direction);
 MC_MoveAbsolute2(Axis:=MC_Axis002, Execute:=bAxis2_Move, Position:=lrAxis2_Pos, Velocity:=lrAxis2_Speed, Acceleration:=5000, Deceleration:=5, Jerk:=300000);
 MC_MoveAbsolute3(Axis:=MC_Axis003, Execute:=bAxis3_Move, Position:=lrAxis3_Pos, Velocity:=lrAxis3_Speed, Acceleration:=3000, Deceleration:=24);//, BufferMode:=_mcBlendingNext);
 MC_MoveAbsolute4(Axis:=MC_Axis004, Execute:=bAxis4_Move, Position:=lrAxis4_Pos, Velocity:=lrAxis4_Speed, Acceleration:=4000, Deceleration:=20);//, BufferMode:=_mcBlendingNext);
 MC_MoveAbsolute5(Axis:=MC_Axis005, Execute:=bAxis5_Move, Position:=lrAxis5_Pos, Velocity:=lrAxis5_Speed, Acceleration:=3000, Deceleration:=10);//, BufferMode:=_mcBlendingNext);
//END_IF;

lrPreError_R := lrError_R;
lrPreError_Z1 := lrError_Z1;
lrPreError_Z2 := lrError_Z2;
lrPreError_Y := lrError_Y;

rPrePgain_Z1 := rPgain_Z1;
rPrePgain_Z2 := rPgain_Z2;
rPrePgain_R := rPgain_R;
rPrePgain_Y := rPgain_Y;

 
  //MC_Move(Axis:=MC_Axis001, Execute:=bAxis1_Vel, Position:=var2, Velocity:=var3, Acceleration:=1, Deceleration:=1, Jerk:=2, Direction:=var7, BufferMode:=var8, MoveMode:=var9, Done=>var10, Busy=>var11, Active=>var12, CommandAborted=>var13, Error=>var14, ErrorID=>var15);
 //MC_SyncMoveVelocity1(Axis:=MC_Axis001, Execute:=bAxis1_Vel, Velocity:=lrAxis1_Speed);
 //MC_MoveRelative1(Axis:=MC_Axis001, Execute:=bAxis1_Vel, Distance:=lrAxis1_Dis, Velocity:=lrAxis1_Speed, Acceleration:=1, Deceleration:=1, Jerk:=2);
 //MC_MoveVelocity1(Axis:=MC_Axis001, Execute:=bAxis1_Vel, Velocity:=lrAxis1_Speed, Acceleration:=2, Deceleration:=2, Jerk:=2, Direction:= lrAxis1_Direction);

