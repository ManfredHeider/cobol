      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU065S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU065S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Jun 14 1996 07:38:38   T13EX14
      *AFKD0094 Neue Satzlänge + AMV monatlich
      *
      *   Rev 1.0   Nov 04 1995 09:51:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   5    20.07.93 10:02:43
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER:  UU065S   =====>>   AM-SACH-BESTANDSSATZ       *
      *                                                                *
      *    ERSTELLT AM   10.08.1982   VON   H. SCHORN  (AGS)           *
      *    GEAEND.  AM   13.10.1988   VON   FR. BIEHL  (DV-FFM):       *
      *                OCCURS VON 3 AUF 4 ERWEITERT BZGL. AMS-SACH-TAB *
      *    GEAEND.  AM   14.05.1990   VON   H. DUONG   (DV-FFM):       *
      *                AMS-F-RES PIC X(15) ===> AMS-F-ANREDE PIC X     *
      *                                         AMS-F-RES    PIC X(14) *
      *    GEAEND.  AM   21.03.1991   VON   H. KUHN    (DV-FFM):       *
      *                AMS-F-RES PIC X(14) ===> AMS-F-AUSL   PIC X(3)  *
      *                                         AMS-F-GEBDAT PIC X(6)  *
      *                                         AMS-F-RES    PIC X(5)  *
      *                                                                *
      *    GEAEND.  AM   17.05.1993   VON   A. BORK    (DV-FFM):       *
      *                UMSTELLUNG AUF NEUE PLZ. DIE NEUE PLZ KOMMT IM  *
      *                FORMAT 9(5) COMP-3 MIT EINEM VORGESCHALTETEN    *
      *                KENNZEICHEN, OB NEUE ODER ALTE PLZ.             *
      *                                                                *
      *                                                                *
      *    GEAEND.  AM   14.06.1996   VON   W. SCHMIDT (DV-FFM):       *
      *             aLLE GEPACKTEN FELDER WERDEN MIT EINEM VOR-        *
      *             zeichen versehen.                                  *
      *             AFKD0094                                           *
      *                                                                *
      *                                                                *
      *    SATZLAENGE:   713 BYTES    VERS 1 VOM 10.08.1982            *
      *    SATZLAENGE:   764 BYTES    VERS 2 VOM 13.10.1988            *
      *    SATZLAENGE:   764 BYTES    VERS 3 VOM 14.05.1990            *
      *    SATZLAENGE:   764 BYTES    VERS 4 VOM 21.03.1991            *
      *    SATZLAENGE:   764 BYTES    VERS 5 VOM 17.05.1993            *
      *    SATZLAENGE:   2174BYTES    VERS 6 VOM 14.06.1996            *
      *                                                                *
      ******************************************************************
CCC009*    SKIP3
       01  UU065S.
CCC009*    SKIP3
           02  AMS-FIX.
CCC009*    SKIP1
      *    >>>>>>>>>>          F I X - T E I L                <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-F-SA            PIC 99.
               03  AMS-F-ABL.
                   04  AMS-F-ABLTT     PIC 99.
                   04  AMS-F-ABLMM     PIC 99.
                   04  AMS-F-ABLJJ     PIC 99.
               03  AMS-F-BEG.
                   04  AMS-F-BEGTT     PIC 99.
                   04  AMS-F-BEGMM     PIC 99.
                   04  AMS-F-BEGJJ     PIC 99.
               03  AMS-F-NAME          PIC X(22).
               03  AMS-F-GEBJ          PIC 99.
170593         03  AMS-F-PLZ-UEBER.
170593*  INHALT 'A' = ALTE PLZ / 'N' = NEUE PLZ
170593             04  AMS-F-PLZ-KZ    PIC X(01).
170593             04  AMS-F-PLZ       PIC S9(05)       COMP-3.
               03  AMS-F-ORT           PIC X(22).
               03  AMS-F-STR           PIC X(23).
               03  AMS-F-VNR           PIC S9(15)       COMP-3.
               03  AMS-F-ZAHLW         PIC X.
               03  AMS-F-VORT          PIC X(22).
140696*        03  AMS-F-VSTR          PIC X(43).
140696         03  AMS-F-VSTR          PIC X(21).
140696         03  AMS-F-DVAG-VERM     PIC 9(06).
140696         03  FILLER              PIC X(12).
140696         03  AMS-F-AMVAGT        PIC 9(04).
               03  AMS-F-FDNR          PIC 9(3).
               03  AMS-F-AGT           PIC 9(4).
               03  AMS-F-ANREDE        PIC X.
               03  AMS-F-AUSL          PIC X(3).
               03  AMS-F-GEBDAT        PIC X(6).
140696         03  AMS-F-vdnr          PIC 9(3).
140696         03  AMS-F-prod-kasko    PIC 9(2).
140696         03  AMS-F-RES           PIC X(150).
140696*        03  AMS-F-RES           PIC X(5).
CCC009*    SKIP3
           02  AMS-VORSATZ  REDEFINES  AMS-FIX.
CCC009*    SKIP1
      *    >>>>>>>>>>           V O R S A T Z                 <<<<<<<<<<
CCC009*    SKIP1
             03  AMS-V-SA              PIC 99.
             03  AMS-V-DATUM.
                 04  AMS-V-TT          PIC 99.
                 04  AMS-V-MM          PIC 99.
                 04  AMS-V-JJ          PIC 99.
             03  AMS-V-TEXT            PIC X(10).
CCC009*    SKIP3
           02  AMS-NACHSATZ  REDEFINES  AMS-FIX.
CCC009*    SKIP1
      *    >>>>>>>>>>         N A C H S A T Z                 <<<<<<<<<<
CCC009*    SKIP1
             03  AMS-N-SA              PIC 99.
             03  AMS-N-ANZAHL          PIC 9(7).
CCC009*    SKIP3
           02  AMS-SACH.
CCC009*    SKIP1
      *    >>>>>>>>>>          S A C H - V E R S .            <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-SACH-TAB               OCCURS 4.
                   04  AMS-S-ZWG       PIC 99.
                   04  AMS-S-ZWGT      PIC X(3).
                   04  AMS-S-BTG       PIC S9(7)V99     COMP-3.
                   04  AMS-S-VGEF      PIC X(21).
                   04  AMS-S-UMF       PIC X(3).
                   04  AMS-S-VSU       PIC S9(9)        COMP-3.
                   04  AMS-S-FI        PIC XX.
                   04  AMS-S-SB        PIC XX.
                   04  AMS-S-VHB       PIC X(5).
                   04  AMS-S-GN        PIC XX.
                   04  AMS-S-KZW       PIC X.
140696         03  ams-s-res           pic x(150).
CCC009*    SKIP3
           02  AMS-KFZMOP.
CCC009*    SKIP1
      *    >>>>>>>>>>            K F Z - V E R S.             <<<<<<<<<<
      *    >>>>>>>>>>        M O P E D - V E R S.             <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-K-ZWG           PIC 99.
               03  AMS-K-ZWGT          PIC X(3).
               03  AMS-K-RUHE          PIC X.
               03  AMS-K-AMTLKZ        PIC X(12).
               03  AMS-K-ZULJAHR       PIC XX.
               03  AMS-K-WKZ           PIC S9(3)        COMP-3.
               03  AMS-K-DH            PIC XX.
               03  AMS-K-BTGH          PIC S9(5)        COMP-3.
               03  AMS-K-KRGJHRH       PIC 9(3).
               03  AMS-K-DF            PIC XX.
               03  AMS-K-BTGF          PIC S9(5)        COMP-3.
               03  AMS-K-KRGJHRF       PIC 9(3).
               03  AMS-K-DU            PIC XX.
               03  AMS-K-BTGU          PIC S9(5)V99     COMP-3.
               03  AMS-K-FNRNAT        PIC X(3).
               03  AMS-K-TARIFB        PIC X(3).
140696         03  ams-k-res           pic x(450).
CCC009*    SKIP3
           02  AMS-HAFT.
CCC009*    SKIP1
      *    >>>>>>>>>>     H A F T P F L I C H T - V E R S .   <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-H-ZWG           PIC 99.
               03  AMS-H-ZWGT          PIC X(3).
               03  AMS-H-BTG           PIC S9(7)V99     COMP-3.
               03  AMS-H-WG1           PIC S9(7)        COMP-3.
               03  AMS-H-WG2           PIC S9(7)        COMP-3.
               03  AMS-H-VSUP          PIC S9(5)        COMP-3.
               03  AMS-H-VSUS          PIC S9(5)        COMP-3.
               03  AMS-H-VSUV          PIC S9(5)        COMP-3.
               03  AMS-H-VSUEK         PIC S9(5)        COMP-3.
               03  AMS-H-WMENGE        PIC S9(7)        COMP-3.
               03  AMS-H-WMTEXT        PIC X(6).
               03  AMS-H-KZW           PIC X.
140696         03  ams-h-res           pic x(150).
CCC009*    SKIP3
           02  AMS-UNFALL.
CCC009*    SKIP1
      *    >>>>>>>>>>       U N F A L L - V E R S .           <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-U-ZWG           PIC 99.
               03  AMS-U-ZWGT          PIC X(3).
               03  AMS-U-BTG           PIC S9(7)V99     COMP-3.
               03  AMS-U-TAB                  OCCURS 5.
                   04  AMS-U-GEBJAHR   PIC 99.
                   04  AMS-U-DU        PIC X.
                   04  AMS-U-TART      PIC XX.
                   04  AMS-U-VSUTOD    PIC S9(7)        COMP-3.
                   04  AMS-U-VSUINV    PIC S9(7)        COMP-3.
                   04  AMS-U-VSUKTG    PIC S9(3)        COMP-3.
                   04  AMS-U-VSUTG     PIC S9(3)        COMP-3.
                   04  AMS-U-VSUGG     PIC S9(3)        COMP-3.
                   04  AMS-U-VSUHK     PIC S9(3)        COMP-3.
140696             04  AMS-U-VSUrente  PIC S9(5)        COMP-3.
140696             04  AMS-U-VSUkur    PIC S9(5)        COMP-3.
140696             04  AMS-U-VSUhhilf  PIC S9(5)        COMP-3.
140696             04  AMS-U-VSUschul  PIC S9(5)        COMP-3.
               03  AMS-U-BG            PIC X.
140696         03  ams-u-res           pic x(150).
CCC009*    SKIP3
           02  AMS-MBUS.
CCC009*    SKIP1
      *    >>>>>>>>>>        M A S C H I N E N - V E R S .    <<<<<<<<<<
      *    >>>>>>>>>>  M A S C H I N E N - B U - V E R S .    <<<<<<<<<<
      *    >>>>>>>>>>          B A U W E S E N - V E R S .    <<<<<<<<<<
      *    >>>>>>>>>>  S C H W A C H S T R O M - V E R S .    <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-MBUS-ZWG        PIC 99.
               03  AMS-MBUS-ZWGT       PIC X(3).
               03  AMS-MBUS-BEDTAB            OCCURS 5.
                   04  AMS-MBUS-BED    PIC XX.
               03  AMS-MBUS-VSU        PIC S9(5)        COMP-3.
               03  AMS-MBUS-TAB               OCCURS 4.
                   04  AMS-MBUS-ART    PIC XX.
                   04  AMS-MBUS-LEIST  PIC XX.
                   04  AMS-MBUS-BTG    PIC S9(7)        COMP-3.
               03  AMS-MBUS-KZW        PIC X.
140696         03  ams-mbus-res        pic x(150).
CCC009*    SKIP3
           02  AMS-TLU.
CCC009*    SKIP1
      *    >>>>>>>>>>      T R A N S P O R T - V E R S .      <<<<<<<<<<
      *    >>>>>>>>>>    L U F T U N F A L L - V E R S .      <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-TLU-ZWG         PIC 99.
               03  AMS-TLU-ZWGT        PIC X(3).
               03  AMS-TLU-TAB                OCCURS 20.
                   04  AMS-TLU-STATKZ  PIC X(3).
               03  AMS-TLU-REG         PIC X.
               03  AMS-TLU-KZW         PIC X.
140696         03  ams-tlu-res         pic x(150).
CCC009*    SKIP3
           02  AMS-TIER.
CCC009*    SKIP1
      *    >>>>>>>>>>         T I E R - V E R S .             <<<<<<<<<<
CCC009*    SKIP1
               03  AMS-T-ZWG           PIC 99.
               03  AMS-T-ZWGT          PIC X(3).
               03  AMS-T-KZ            PIC X.
               03  AMS-T-BTG           PIC S9(5)        COMP-3.
               03  AMS-T-TAB                  OCCURS 7.
                   04  AMS-T-ART       PIC 99.
                   04  AMS-T-GAT       PIC 99.
                   04  AMS-T-VZW       PIC 99.
               03  AMS-T-KZW           PIC X.
CCC009*    SKIP1
      ********************  ENDE  COPY-MEMBER  UU065S  *****************

