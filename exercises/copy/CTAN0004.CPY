      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0004.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/CTAN0004.CPv  $
      *
      *   Rev 2.8   Nov 19 2015 08:04:56   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.7   Nov 04 2015 13:32:20   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.6   May 28 2015 09:55:28   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.5   Apr 23 2015 13:25:50   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   27 Dec 2007 11:38:26   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 70 BADENIA
      *
      *   Rev 2.3   01 Sep 2006 09:49:46   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.2   31 Aug 2006 09:44:06   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Mar 06 2003 16:53:30   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Feb 27 1999 11:05:12   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   09 Sep 1997 09:13:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0004                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0004 VIA-BADENIA
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03!2.1  !NEU                        !RAHNER   *
      * AAN00684!31.08.06!2.2  !KUNDENBESTANDS-ID FÜR AVS  !RAHNER   *
      * AAN00730!28.12.07!2.4  !ALLFINANZ                  !RAHNER   *
      * AAN01211!06.03.15!2.5  !ESIGN                      !HEIDER   *
      * AAN01211!21.10.15!2.7  !XGDV AUF 30.000            !HEIDER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0004 TABLE
CCC004*    (
CCC004*       VIA_HVANTNUM                   DECIMAL (17, 0),
CCC004*       VIA_PIN                        INT,
CCC004*       VIA_PIN_2                      INT,
CCC004*       VIA_VDABVERM                   INT,
CCC004*       VIA_LAST_UPDATE                TIMESTAMP,
CCC004*       VIA_KENNUNG_AEND               CHAR (8),
CCC004*       VIA_KENNUNG_BSV                CHAR (1),
CCC004*       VIA_KENNUNG_TARIF              CHAR (1),
CCC004*       VIA_KENNUNG_FRST               CHAR (1),
CCC004*       VIA_KENNUNG_LEG                CHAR (1),
CCC004*       VIA_KENNUNG_VL                 CHAR (1),
CCC004*       VIA_KENNUNG_VL2                CHAR (1),
CCC004*       VIA_KENNUNG_ERH                CHAR (1),
CCC004*       VIA_HAND_BSV                   CHAR (1),
CCC004*       VIA_HAND_TARIF                 CHAR (1),
CCC004*       VIA_HAND_FRST                  CHAR (1),
CCC004*       VIA_HAND_LEG                   CHAR (1),
CCC004*       VIA_HAND_VL                    CHAR (1),
CCC004*       VIA_HAND_VL2                   CHAR (1),
CCC004*       PGEB_GES                       SMALLINT,
CCC004*       VIA_VDABVERMPZ                 CHAR (1),
CCC004*       VIA_VDVMNR                     INT,
CCC004*       VIA_VDVMNRPZ                   CHAR (1),
CCC004*       VIA_VDVMZUNA                   CHAR (26),
CCC004*       VIA_VDVMVORNA                  CHAR (26),
CCC004*       VIA_VDDIREKT                   SMALLINT,
CCC004*       VIA_PDVNZUNA                   CHAR (26),
CCC004*       VIA_PDVNVORNA                  CHAR (26),
CCC004*       VIA_PDVNPLZ                    CHAR (6),
CCC004*       VIA_PDVNORT                    CHAR (30),
CCC004*       VIA_PDVNLKZ                    CHAR (3),
CCC004*       VIA_VDPRODKEY                  SMALLINT,
CCC004*       VIA_DVAG_PRODKEY               SMALLINT,
CCC004*       VIA_VDABLEIN                   DECIMAL (7, 1),
CCC004*       VIA_SUMME                      DECIMAL (11, 2),
CCC004*       VIA_ANTRAG_DATUM               DATE,
CCC004*       VIA_VERSION                    SMALLINT,
CCC004*       VIA_ERSTELLUNG                 TIMESTAMP,
CCC004*       VIA_KENNUNG_ERST               CHAR (8),
CCC004*       VIA_XGDV                       VARCHAR (30000),
CCC004*       VIA_KD_BEST_ID                 DECIMAL (18, 0),
CCC004*       VIA_VB_GES                     SMALLINT,
CCC004*       VIA_ESIGN                      CHAR (1)
 
CCC004*    )
CCC003*    END-EXEC.
 
 
       01  TBAN0004-GESAMT.
           05 DCL-TBAN0004.
              10 VIA-HVANTNUM                   PIC S9(17) COMP-3.
              10 VIA-PIN                        PIC S9(9) COMP.
              10 VIA-PIN-2                      PIC S9(9) COMP.
              10 VIA-VDABVERM                   PIC S9(9) COMP.
              10 VIA-LAST-UPDATE                PIC X(26).
              10 VIA-KENNUNG-AEND               PIC X(8).
              10 VIA-KENNZ-BSV                  PIC X.
              10 VIA-KENNZ-TARIF                PIC X.
              10 VIA-KENNZ-FRST                 PIC X.
              10 VIA-KENNZ-LEG                  PIC X.
              10 VIA-KENNZ-VL                   PIC X.
              10 VIA-KENNZ-VL2                  PIC X.
              10 VIA-KENNZ-ERH                  PIC X.
              10 VIA-HAND-BSV                   PIC X.
              10 VIA-HAND-TARIF                 PIC X.
              10 VIA-HAND-FRST                  PIC X.
              10 VIA-HAND-LEG                   PIC X.
              10 VIA-HAND-VL                    PIC X.
              10 VIA-HAND-VL2                   PIC X.
              10 PGEB-GES                       PIC S9(4) COMP.
              10 VIA-VDABVERMPZ                 PIC X.
              10 VIA-VDVMNR                     PIC S9(9) COMP.
              10 VIA-VDVMNRPZ                   PIC X.
              10 VIA-VDVMZUNA                   PIC X(26).
              10 VIA-VDVMVORNA                  PIC X(26).
              10 VIA-VDDIREKT                   PIC S9(4) COMP.
              10 VIA-PDVNZUNA                   PIC X(26).
              10 VIA-PDVNVORNA                  PIC X(26).
              10 VIA-PDVNPLZ                    PIC X(6).
              10 VIA-PDVNORT                    PIC X(30).
              10 VIA-PDVNLKZ                    PIC X(3).
              10 VIA-VDPRODKEY                  PIC S9(4) COMP.
              10 VIA-DVAG-PRODKEY               PIC S9(4) COMP.
              10 VIA-VDABLEIN                   PIC S9(6)V9(1) COMP-3.
              10 VIA-SUMME                      PIC S9(9)V9(2) COMP-3.
              10 VIA-ANTRAG-DATUM               PIC X(10).
              10 VIA-VERSION                    PIC S9(4) COMP.
              10 VIA-ERSTELLUNG                 PIC X(26).
              10 VIA-KENNUNG-ERST               PIC X(8).
              10 VIA-XGDV.
                 49 VIA-XGDV-LEN                PIC S9(4) COMP.
                 49 VIA-XGDV-TEXT               PIC X(30000).
310806        10 VIA-KD-BEST-ID                 PIC S9(18)V COMP-3.
281207        10 VIA-VB-GES                     PIC S9(4) COMP.
060315        10 VIA-ESIGN                      PIC X(1).
 
 

