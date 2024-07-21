      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0024.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRRW0024.CPv  $
      *
      *   Rev 2.2   Jun 01 2017 11:03:16   t13ex18
      *ASL00567: LUCY2: erweitern Kostenstelle 4->6 Stellen(gbu)
      *
      *   Rev 2.1   Jul 19 2016 12:09:28   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Korrektur STAB UPRW0006/UPRW0
      *007 (HWO)
      *
      *   Rev 2.0   Jul 14 2016 11:56:02   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0024                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    14.07.2016                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!14.07.16!2.1 ! NEU                       ! H. WÖLFLIK*
      *         !        !    !                           !           *
      * AVB01450!30.05.17!2.2 ! Kostenstelle 4->6 Stellen ! G. Bruder *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRRW0024 - Korrektur FIBU-Schnittstelle UPRW0006  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRRW0024.
      ***  Daten
           05  PI-DRRW0024-DATEN.
               10  PI-DRRW0024-MA-ST-NR         PIC S9(009)    BINARY.
               10  PI-DRRW0024-PZ               PIC  X(001).
               10  PI-DRRW0024-BETR-UPFL-NETTO  PIC S9(09)V99.
               10  PI-DRRW0024-BETR-UPFL-BRUTTO PIC S9(09)V99.
               10  PI-DRRW0024-UST-ANTEIl-U     PIC S9(09)V99.
               10  PI-DRRW0024-TURNUS-JJJJMM    PIC  9(006).
               10  FILLER REDEFINES PI-DRRW0024-TURNUS-JJJJMM.
                   15 PI-DRRW0024-TURNUS-JJJJ.
                      20 PI-DRRW0024-TURNUS-JH  PIC  9(002).
                      20 PI-DRRW0024-TURNUS-JJ  PIC  9(002).
                   15 PI-DRRW0024-TURNUS-MM     PIC  9(002).
               10  PI-DRRW0024-GESELLSCHAFT     PIC  X(012).
               10  PI-DRRW0024-BUCHTEXT         PIC  X(035).
               10  PI-DRRW0024-MANDANT          PIC  X(003).
               10  PI-DRRW0024-BELEGART         PIC  X(003).
300517         10  PI-DRRW0024-KOSTENSTELLE     PIC  X(006).
               10  PI-DRRW0024-KONTO-SOLL       PIC  X(012).
               10  PI-DRRW0024-KONTO-HABEN      PIC  X(012).
               10  PI-DRRW0024-LISTE            PIC  X(005).
               10  PI-DRRW0024-JOB              PIC  X(006).
               10  PI-DRRW0024-STEUERJAHR       PIC  9(004).
               10  PI-DRRW0024-BEZUGSJAHR       PIC  9(004).
               10  PI-DRRW0024-BUCHTEXT-FIBU    PIC  X(080).
               10  PI-DRRW0024-ABR-NR           PIC  X(015).
               10  PI-DRRW0024-BELEGDATUM       PIC  X(008).
               10  PI-DRRW0024-BUCH-VERMERK     PIC  X(002).
               10  PI-DRRW0024-KTY-VERARB-TYP   PIC S9(004)    BINARY.
               10  PI-DRRW0024-KNR-VERARB-TYP   PIC S9(004)    BINARY.
               10  PI-DRRW0024-BEZUGS-LFD       PIC  9(016).
               10  PI-DRRW0024-ZIELSTATUS       PIC  X(001).
                   88 PI-DRRW0024-ZIELSTATUS-K       VALUE 'K'.
                   88 PI-DRRW0024-ZIELSTATUS-U       VALUE 'U'.
               10  PI-DRRW0024-SOLL-HABEN       PIC  X(001).
                   88 PI-DRRW0024-SOLL               VALUE 'S'.
                   88 PI-DRRW0024-HABEN              VALUE 'H'.
      *
       01  PO-DRRW0024.
         03  PO-DRRW0024-RC                     PIC  X(03).
               88  PO-DRRW0024-RC-OK                 VALUE 'OK '.
               88  PO-DRRW0024-RC-NOT-OK             VALUE 'NOK'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRRW0024   --------------------------*
      *---------------------------------------------------------------*
 
 

