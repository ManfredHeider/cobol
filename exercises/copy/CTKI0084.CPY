000010*---------------------------------------------------------------*
000020*    Standard fuer alle Objekte unter PVCS VM
000030*            Datum      :  05/10/15
000040*            Kategorie  :  Copystrecke
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   CTKI0084.CPY  $
000110*    $Revision:   2.5  $
000120*
000130*    $Log:   L:/mf/CPY/CTKI0084.CPv  $
000140*
000150*   Rev 2.5   Jan 11 2016 16:55:58   t13ex78
000160*AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - Er
000170*weiterung ZTKI0084 um KUBERLEDIGTKZ
000180*
000190*   Rev 2.4   Nov 09 2015 17:04:56   t13ex78
000200*AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
000210*andard-OZP für TCKI0084
000220*
000230*   Rev 2.3   Nov 09 2015 16:50:46   t13ex78
000240*AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
000250*andard-OZP für TCKI0084
000260*
000270*   Rev 2.2   Oct 28 2015 09:18:56   t13ex78
000280*AKI01382: CoC neue KÜ, Blacklist, Completefunktion ect. - St
000290*andard-OZP für TCKI0084
000300*
000310*   Rev 2.0   Oct 5 2015 13:24:22   T13EX78
000320*Archiverstellung
000330*
000340*---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE KUEBLACKLIST TABLE
CCC004*    ( KUBPERSONENID                  CHAR(13) NOT NULL,
CCC004*      KUBVBID                        CHAR(13) NOT NULL,
CCC004*      KUBERLEDIGTKZ                  CHAR(1) NOT NULL,
CCC004*      KUBTICKETID                    DECIMAL(18, 0) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
000420******************************************************************
000430* COBOL DECLARATION FOR TABLE KUEBLACKLIST                *
000440******************************************************************
000450 01  KUEBLACKLIST-GESAMT.
000460  05 NI-KUEBLACKLIST-TAB.
000470     10 NI-KUEBLACKLIST-ELE PIC S9(04) COMP OCCURS 5 TIMES.
000480  05 NI-KUEBLACKLIST REDEFINES NI-KUEBLACKLIST-TAB.
000490     10 NI-KUBPERSONENID      PIC S9(4) COMP.
000500     10 NI-KUBVBID            PIC S9(4) COMP.
000510     10 NI-KUBERLEDIGTKZ      PIC S9(4) COMP.
000520     10 NI-KUBTICKETID        PIC S9(4) COMP.
000530     10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
000540  05 DCL-KUEBLACKLIST.
000550     10 KUBPERSONENID        PIC X(13).
000560     10 KUBVBID              PIC X(13).
000570     10 KUBERLEDIGTKZ        PIC X(1).
000580     10 KUBTICKETID          PIC S9(18)V USAGE COMP-3.
000590     10 LETZTEAENDERUNG      PIC X(26).
000600************************** ENDE-DCLGEN *************************

