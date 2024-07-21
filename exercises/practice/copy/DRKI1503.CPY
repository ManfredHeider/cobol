      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1503.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1503.CPv  $
      *
      *   Rev 2.1   Mar 26 2003 17:16:16   t13ae44
      *AKI00329 - Auswertung Kd-/Vertragsbest. AMV u. AML
      *
      *   Rev 2.0   Jan 13 2003 15:47:40   p13sp16
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER Auswertung Kd-/Vertragsbestand AML,AMV     *
      *                                                                *
      *    ----------------------------------------------------------- *
      *                                                                *
      *    LAENGE =  75 Bytes       ERSTELLT AM 05.02.03.              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                    !NAME     !DATUM     *
      *AUFTRAG !                                  !         !          *
      *--------!----------------------------------!---------!----------*
      *AKI00329!Neuerstellung                     ! R.Biehl !05.02.2003*
      *        !                                  !         !          *
      *        !                                  !         !          *
      ******************************************************************
       01  DRKI1503.
         05 DRKI1503-DATEN.
           10  MONAT      PIC X(2).
           10  TRENN      PIC X.
           10  JAHR       PIC X(4).
           10  TRENN0     PIC X.
           10  VBNR       PIC X(7).
           10  TRENN1     PIC X.
           10  KD-GES     PIC X(7).
           10  TRENN2     PIC X.
           10  KD-AML     PIC X(7).
           10  TRENN3     PIC X.
           10  KD-AMV     PIC X(7).
           10  TRENN4     PIC X.
           10  VERTR-AML  PIC X(7).
           10  TRENN5     PIC X.
           10  VERTR-AMV  PIC X(7).
           10  TRENN6     PIC X.
           10  JBEITR-AML PIC X(9).
           10  TRENN7     PIC X.
           10  JBEITR-AMV PIC X(9).
      *---------------------------------------------------------------*
