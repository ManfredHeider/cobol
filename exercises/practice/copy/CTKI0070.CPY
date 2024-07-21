      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0070.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0070.CPv  $
      *
      *   Rev 2.4   May 24 2018 10:24:48   t13ex78
      *AKI01587: OZP's
      *
      *   Rev 2.3   Dec 06 2016 15:13:36   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.2   Nov 28 2016 12:16:24   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.1   Nov 28 2016 11:35:00   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.0   Feb 27 1999 12:50:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   09 May 1998 13:27:56   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE VBSTAMMDATEN TABLE
CCC004*    ( VBSPERSONENID                  CHAR(13) NOT NULL,
CCC004*      VBSVORGESPERSID                CHAR(13),
CCC004*      VBSMITARBEITERNR               INTEGER NOT NULL,
CCC004*      VBSMANDANTNR                   INTEGER NOT NULL,
CCC004*      VBSANREDE                      SMALLINT NOT NULL,
CCC004*      VBSNAME                        CHAR(40) NOT NULL,
CCC004*      VBSVORNAME                     CHAR(20),
CCC004*      VBSGEBGRUENDDAT                DATE,
CCC004*      VBSSTRUKTURSTUFE               INTEGER,
CCC004*      VBSDIREKTION                   SMALLINT NOT NULL,
CCC004*      VBSPROVSATZ                    DECIMAL(5, 2),
CCC004*      VBSAUSTRITTSDATUM              DATE,
CCC004*      VBSRUECKSTUFDATUM              DATE,
CCC004*      VBSUMSTUFDATUM                 DATE,
CCC004*      VBS34FDAT                      DATE,
CCC004*      VBS34FREGISTNR                 CHAR(15),
CCC004*      VBS34IDAT                      DATE,
CCC004*      VBS34CDAT                      DATE,
CCC004*      VBSEDISTATUS                   CHAR(1),
CCC004*      VBSEDIDATUM                    DATE,
CCC004*      VBSZUSTELLPRIV                 SMALLINT,
CCC004*      VBSZUSTELLGESCH                SMALLINT,
CCC004*      VBSFONPRIVAT                   CHAR(26),
CCC004*      VBSFAXPRIVAT                   CHAR(26),
CCC004*      VBSMOBILPRIVAT                 CHAR(26),
CCC004*      VBSSTRASSENRPRIV               CHAR(40),
CCC004*      VBSPLZPRIV                     CHAR(9),
CCC004*      VBSORTPRIV                     CHAR(40),
CCC004*      VBSAUSLPRIV                    SMALLINT,
CCC004*      VBSFONGESCH                    CHAR(26),
CCC004*      VBSFAXGESCH                    CHAR(26),
CCC004*      VBSMOBILGESCH                  CHAR(26),
CCC004*      VBSSTRASSENRGESCH              CHAR(40),
CCC004*      VBSPLZGESCH                    CHAR(9),
CCC004*      VBSORTGESCH                    CHAR(40),
CCC004*      VBSAUSLGESCH                   SMALLINT,
CCC004*      VBSMANDANTNRALT                INTEGER,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      VBSVBARTKZ                     CHAR(1) NOT NULL,
CCC004*      VBS34DREGISTNR                 CHAR(15),
CCC004*      VBS34IREGISTNR                 CHAR(15)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VBSTAMMDATEN                *
      ******************************************************************
       01  VBSTAMMDATEN-GESAMT.
        05 NI-VBSTAMMDATEN-TAB.
           10 NI-VBSTAMMDATEN-ELE PIC S9(04) COMP OCCURS 41 TIMES.
        05 NI-VBSTAMMDATEN REDEFINES NI-VBSTAMMDATEN-TAB.
           10 NI-VBSPERSONENID      PIC S9(4) COMP.
           10 NI-VBSVORGESPERSID    PIC S9(4) COMP.
           10 NI-VBSMITARBEITERNR   PIC S9(4) COMP.
           10 NI-VBSMANDANTNR       PIC S9(4) COMP.
           10 NI-VBSANREDE          PIC S9(4) COMP.
           10 NI-VBSNAME            PIC S9(4) COMP.
           10 NI-VBSVORNAME         PIC S9(4) COMP.
           10 NI-VBSGEBGRUENDDAT    PIC S9(4) COMP.
           10 NI-VBSSTRUKTURSTUFE   PIC S9(4) COMP.
           10 NI-VBSDIREKTION       PIC S9(4) COMP.
           10 NI-VBSPROVSATZ        PIC S9(4) COMP.
           10 NI-VBSAUSTRITTSDATUM  PIC S9(4) COMP.
           10 NI-VBSRUECKSTUFDATUM  PIC S9(4) COMP.
           10 NI-VBSUMSTUFDATUM     PIC S9(4) COMP.
           10 NI-VBS34FDAT          PIC S9(4) COMP.
           10 NI-VBS34FREGISTNR     PIC S9(4) COMP.
           10 NI-VBS34IDAT          PIC S9(4) COMP.
           10 NI-VBS34CDAT          PIC S9(4) COMP.
           10 NI-VBSEDISTATUS       PIC S9(4) COMP.
           10 NI-VBSEDIDATUM        PIC S9(4) COMP.
           10 NI-VBSZUSTELLPRIV     PIC S9(4) COMP.
           10 NI-VBSZUSTELLGESCH    PIC S9(4) COMP.
           10 NI-VBSFONPRIVAT       PIC S9(4) COMP.
           10 NI-VBSFAXPRIVAT       PIC S9(4) COMP.
           10 NI-VBSMOBILPRIVAT     PIC S9(4) COMP.
           10 NI-VBSSTRASSENRPRIV   PIC S9(4) COMP.
           10 NI-VBSPLZPRIV         PIC S9(4) COMP.
           10 NI-VBSORTPRIV         PIC S9(4) COMP.
           10 NI-VBSAUSLPRIV        PIC S9(4) COMP.
           10 NI-VBSFONGESCH        PIC S9(4) COMP.
           10 NI-VBSFAXGESCH        PIC S9(4) COMP.
           10 NI-VBSMOBILGESCH      PIC S9(4) COMP.
           10 NI-VBSSTRASSENRGESCH  PIC S9(4) COMP.
           10 NI-VBSPLZGESCH        PIC S9(4) COMP.
           10 NI-VBSORTGESCH        PIC S9(4) COMP.
           10 NI-VBSAUSLGESCH       PIC S9(4) COMP.
           10 NI-VBSMANDANTNRALT    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-VBSVBARTKZ         PIC S9(4) COMP.
           10 NI-VBS34DREGISTNR     PIC S9(4) COMP.
           10 NI-VBS34IREGISTNR     PIC S9(4) COMP.
        05 DCL-VBSTAMMDATEN.
           10 VBSPERSONENID        PIC X(13).
           10 VBSVORGESPERSID      PIC X(13).
           10 VBSMITARBEITERNR     PIC S9(9) USAGE COMP.
           10 VBSMANDANTNR         PIC S9(9) USAGE COMP.
           10 VBSANREDE            PIC S9(4) USAGE COMP.
           10 VBSNAME              PIC X(40).
           10 VBSVORNAME           PIC X(20).
           10 VBSGEBGRUENDDAT      PIC X(10).
           10 VBSSTRUKTURSTUFE     PIC S9(9) USAGE COMP.
           10 VBSDIREKTION         PIC S9(4) USAGE COMP.
           10 VBSPROVSATZ          PIC S9(3)V9(2) USAGE COMP-3.
           10 VBSAUSTRITTSDATUM    PIC X(10).
           10 VBSRUECKSTUFDATUM    PIC X(10).
           10 VBSUMSTUFDATUM       PIC X(10).
           10 VBS34FDAT            PIC X(10).
           10 VBS34FREGISTNR       PIC X(15).
           10 VBS34IDAT            PIC X(10).
           10 VBS34CDAT            PIC X(10).
           10 VBSEDISTATUS         PIC X(1).
           10 VBSEDIDATUM          PIC X(10).
           10 VBSZUSTELLPRIV       PIC S9(4) USAGE COMP.
           10 VBSZUSTELLGESCH      PIC S9(4) USAGE COMP.
           10 VBSFONPRIVAT         PIC X(26).
           10 VBSFAXPRIVAT         PIC X(26).
           10 VBSMOBILPRIVAT       PIC X(26).
           10 VBSSTRASSENRPRIV     PIC X(40).
           10 VBSPLZPRIV           PIC X(9).
           10 VBSORTPRIV           PIC X(40).
           10 VBSAUSLPRIV          PIC S9(4) USAGE COMP.
           10 VBSFONGESCH          PIC X(26).
           10 VBSFAXGESCH          PIC X(26).
           10 VBSMOBILGESCH        PIC X(26).
           10 VBSSTRASSENRGESCH    PIC X(40).
           10 VBSPLZGESCH          PIC X(9).
           10 VBSORTGESCH          PIC X(40).
           10 VBSAUSLGESCH         PIC S9(4) USAGE COMP.
           10 VBSMANDANTNRALT      PIC S9(9) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 VBSVBARTKZ           PIC X(1).
           10 VBS34DREGISTNR       PIC X(15).
           10 VBS34IREGISTNR       PIC X(15).
      ************************** ENDE-DCLGEN *************************

