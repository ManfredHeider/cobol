      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.01.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3622.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3622.CPv  $
      *
      *   Rev 2.2   Jan 22 2020 10:48:46   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen 
      *
      *   Rev 2.1   Mar 18 2019 17:49:10   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.0   Jan 25 2019 12:47:26   t13ae52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3622                                      *
      *    --------------------------------------------------------   *
      *    CURSOR FULL SELECT ZXKI0529 G48_KONTOKORRENT               *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01658!25.01.19!2.0 ! NEUERSTELLUNG             !SCHUMACHER*
      *---------!--------!--------------------------------!----------*
      * AKI01754!04.01.20! NEUES ATTRIBUT "ULTIMOSPAREN"  ! T13EX39
      *         !        !                                !
      *         !        !                                !
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3622
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3622.
           05 PI-DRKI3622-UFKT-CODE             PIC  9(0002).
              88 PI-DRKI3622-UFKT-FIRST              VALUE 1.
              88 PI-DRKI3622-UFKT-NEXT               VALUE 2.
              88 PI-DRKI3622-UFKT-QUIT               VALUE 3.
 
           05 PI-DRKI3622-KEY.
              10 PI-DRKI3622-G48MANDANT         PIC  S9(9) COMP.


       01  PO-DRKI3622.
           05 PO-DRKI3622-KEY.
              10 PO-DRKI3622-G48KKOVNR          PIC  X(0015).
              10 PO-DRKI3622-G48KKOUKONTONR     PIC  X(0002).
              10 PO-DRKI3622-G48KKOWAEHRUNG     PIC  X(0003).
 
           05 PO-DRKI3622-RC                    PIC  X(0003).
              88 PO-DRKI3622-RC-OK              VALUE 'OK '.
              88 PO-DRKI3622-RC-LEER            VALUE 'LER'.
              88 PO-DRKI3622-RC-EOF             VALUE 'EOF'.
      *
           05 PO-DRKI3622-DATEN.
              10 PO-DRKI3622-G48KKOBIC          PIC  X(0011).
              10 PO-DRKI3622-G48KKOBILANZSCHL   PIC  X(0004).
              10 PO-DRKI3622-G48KKODATENDEBEG   PIC  X(0008).
              10 PO-DRKI3622-G48KKODATOEFF      PIC  X(0008).
              10 PO-DRKI3622-G48KKODATSALDO     PIC  X(0008).
              10 PO-DRKI3622-G48KKODELKZ        PIC  X(0001).
              10 PO-DRKI3622-G48KKOEXTLIMIT     PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOHABENZINS    PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3622-G48KKOIBAN         PIC  X(0034).
              10 PO-DRKI3622-G48KKOIBANKK       PIC  X(0034).
              10 PO-DRKI3622-G48KKOINHABER      PIC  X(0028).
              10 PO-DRKI3622-G48KKOKENNZSCHL    PIC  X(0002).
              10 PO-DRKI3622-G48KKOKFWDATFEST   PIC  X(0008).
              10 PO-DRKI3622-G48KKOKFWERSTR     PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOKFWFOLGER    PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOKFWKREDITB   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOKFWSOLLZ     PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3622-G48KKOKFWTILGUNG   PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3622-G48KKOKONDSCHL     PIC  X(0002).
              10 PO-DRKI3622-G48KKOKOSTSCHL     PIC  X(0001).
              10 PO-DRKI3622-G48KKOKUMHABENAM   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOKUMHABENVM   PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOKUMSOLLAM    PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOKUMSOLLVM    PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOMANDREF      PIC  X(0035).
              10 PO-DRKI3622-G48KKOPIN          PIC  X(0009).
              10 PO-DRKI3622-G48KKOSALDO        PIC S9(12)V9(2) COMP-3.
              10 PO-DRKI3622-G48KKOSCHLDAT      PIC  X(0008).
              10 PO-DRKI3622-G48KKOSOLLZINS     PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3622-G48KKOUEBERZINS    PIC S9(2)V9(4) COMP-3.
              10 PO-DRKI3622-G48KKOZRAM         PIC  X(0005).
              10 PO-DRKI3622-G48KKOZRVM         PIC  X(0005).
              10 PO-DRKI3622-G48KKOULTOSPARKZ   PIC  X(0001).
      ******* ENDE COPY-MEMBER ***************************************



