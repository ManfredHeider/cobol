      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU023S.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/UU023S.CPv  $
      *
      *   Rev 2.3   06 Sep 2006 08:56:18   T13AE23
      *AAN00678: Bereitstellung des VBAS Kennzeichen 
      *
      *   Rev 2.2   29 Aug 2006 14:39:06   T13AE23
      *FPA00848: Dummy für PA-Umgebung -> AAN00614
      *
      *   Rev 2.1   Mar 25 2003 15:50:46   t13ae34
      *AAN00432-SST AML Provisionsstrukturen DTA261 und Z1510
      *
      *   Rev 2.0   Feb 27 1999 15:15:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:47:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                                                    *
      *        COPY-MEMBER VERAENDERUNGSSATZ               *
      *        -----------------------------               *
      *                                                    *
      *                        ERSTELLT AM 24. JUNI 1980   *
      *                        VON E. HUEBNER / GEDO       *
      *                                                    *
      *                        AG8DATEN VON 11 AUF 18 ERW. *
      *                        DOBSLAFF 02.03.88           *
      *                                                    *
      *                        2.STELLE VON GES1 REDEF.    *
      *                        HECKER   11.01.91           *
      ******************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AAN00432!11.03.03!2.1  ! AML SST PE-STRUKTUREN    ! HEIDER   *
      * AAN00678!29.08.06!2.2  ! KENNZEICHEN F. VBAS      ! RAHNER   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  UU023S.
           05  VSATZ.
               10  SART            PIC 999.
               10  GES      REDEFINES   SART.
                   20  GES1        PIC 99.
                   20  FILLER   REDEFINES GES1.
                       30  FILLER  PIC 9.
                       30  VM-KZ   PIC 9.
                   20  FILLER      PIC X.
               10  VERNR.
                   20  VNR         PIC 9(10).
                   20  FILLER      PIC X(10).
               10  VERNR1    REDEFINES   VERNR.
                   20  VNR1        PIC 9(12).
                   20  FILLER      PIC X(8).
               10  NAME            PIC X(34).
290806         10  VBAS-KZ         PIC X.
290806*        10  FILLER          PIC X(62).
290806         10  FILLER          PIC X(61).
               10  TERMIN.
                   20  NAEN        PIC 9(4).
                   20  DATMM       PIC 99.
                   20  DATJJ       PIC 99.
               10  FILLER          PIC X(21).
               10  ORGGS           PIC 9(7).
               10  FILLER          PIC X(46).
               10  TARIF           PIC XX.
               10  FILLER          PIC X(336).
               10  ABSCHLD.
                   20  FILLER      PIC X(18).
                   20  ANZPROV     PIC 99.
               10  AG8DATEN        OCCURS 18.
                   20  VBNR        PIC 9(7).
                   20  VBNR8       PIC 9.
                   20  PRZ         PIC X.
                   20  PROV        PIC S999V99     COMP-3.
                   20  FILLER      PIC XXX.
               10  FILLER          PIC X(411).
               10  BEWSCHL.
                   20  BEWART      PIC 9(4).
110303*            20  BEWRTMM     PIC 99.
110303*            20  BEWRTJJ     PIC 99.
110303             20  BEWRTDAT.
110303                 25  BEWRTTT     PIC 99.
110303                 25  BEWRTMM     PIC 99.
110303                 25  BEWRTJH     PIC 99.
110303                 25  BEWRTJJ     PIC 99.
110303*        10  FILLER          PIC X(37).
110303         10  FILLER          PIC X(33).

