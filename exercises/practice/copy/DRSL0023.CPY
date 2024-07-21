      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0023.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRSL0023.CPv  $
      *
      *   Rev 2.2   Jun 23 2015 17:19:30   t13ae35
      *ASL00537: Familienabsicherungsplan - Prüfung der Selbstbetei
      *ligungen - Verbesserungen
      *
      *   Rev 2.1   Jun 18 2015 15:48:28   t13ae35
      *ASL00537: Familienabsicherungsplan - Prüfung der Selbstbetei
      *ligungen - Prüfroutine
      *
      *   Rev 2.0   Jun 04 2015 16:54:16   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRSL0023                                      *
      *    --------------------------------------------------------   *
      *    PI/PO für UPSL0002 (FAP-Bedingung erfüllt)                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Hering                                          *
      *    DATUM:    09.06.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00537!09.06.15! 2.1! NEU                       ! HERING   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRSL0023
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRSL0023.
           05  PI-DRSL0023-STICHTAG-ISO       PIC  X(010).
           05  PI-DRSL0023-GESELL             PIC  9(002).
           05  PI-DRSL0023-VBNR               PIC S9(09)       BINARY.
           05  PI-DRSL0023-FAP-STF            PIC  X(001).
           05  PI-DRSL0023-DVAG-STF           PIC  X(001).
160615     05  PI-DRSL0023-VB-STF             PIC  9(003).
160615     05  PI-DRSL0023-PR-STF             PIC  9(001).
      *
       01  PO-DRSL0023.
           05  PO-DRSL0023-RC                 PIC  X(003).
               88 PO-DRSL0023-RC-OK                        VALUE 'OK '.
               88 PO-DRSL0023-RC-NOK                       VALUE 'NOK'.
           05  PO-DRSL0023-RUECK-GES.
               10 PO-DRSL0023-FAP-ERF         PIC  X(001).
                   88 PO-DRSL0023-FAP-ERF-JA         VALUE 'J'.
                   88 PO-DRSL0023-FAP-ERF-NEIN       VALUE 'N'.
               10 PO-DRSL0023-EH-GES-ERF      PIC  X(001).
                   88 PO-DRSL0023-EH-GES-ERF-JA      VALUE 'J'.
                   88 PO-DRSL0023-EH-GES-ERF-NEIN    VALUE 'N'.
               10 PO-DRSL0023-EQF-GES-ERF     PIC  X(001).
                   88 PO-DRSL0023-EQF-GES-ERF-JA      VALUE 'J'.
                   88 PO-DRSL0023-EQF-GES-ERF-NEIN    VALUE 'N'.
               10 PO-DRSL0023-EH-EIG-ERF      PIC  X(001).
                   88 PO-DRSL0023-EH-EIG-ERF-JA      VALUE 'J'.
                   88 PO-DRSL0023-EH-EIG-ERF-NEIN    VALUE 'N'.
               10 PO-DRSL0023-EQF-EIG-ERF     PIC  X(001).
                   88 PO-DRSL0023-EQF-EIG-ERF-JA      VALUE 'J'.
                   88 PO-DRSL0023-EQF-EIG-ERF-NEIN    VALUE 'N'.
160615         10 PO-DRSL0023-VB-STF-ERF   PIC  X(001).
160615             88 PO-DRSL0023-VB-STF-ERF-JA    VALUE 'J'.
160615             88 PO-DRSL0023-VB-STF-ERF-NEIN  VALUE 'N'.
 
220615         10 PO-DRSL0023-AS-CLUB      PIC  X(001).
220615             88 PO-DRSL0023-AS-CLUB-JA       VALUE 'J'.
220615             88 PO-DRSL0023-AS-CLUB-NEIN     VALUE 'N'.
 
           05  PO-DRSL0023-RUECK-FAKTEN.
               10  PO-DRSL0023-EH-GES-3MON
                             PIC  S9(009)V9  COMP-3.
               10  PO-DRSL0023-EH-GES-12MON
                             PIC  S9(009)V9  COMP-3.
               10  PO-DRSL0023-EH-GES-12MON-KOR
                             PIC  S9(009)V9  COMP-3.
               10  PO-DRSL0023-EQF-GES
                             PIC  S9(003)V99 COMP-3.
               10  PO-DRSL0023-EH-EIG-12MON
                             PIC  S9(009)V9  COMP-3.
               10  PO-DRSL0023-EH-EIG-12MON-NEU
                             PIC  S9(009)V9  COMP-3.
               10  PO-DRSL0023-EQF-EIG
                             PIC  S9(003)V99 COMP-3.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

