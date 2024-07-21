      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3055.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3055.CPv  $
      *
      *   Rev 2.1   Mar 14 2016 09:56:32   t13ex27
      *AKI01400: Redesign UPKI0203(Kundenübertragung)-UP für Änderu
      *ng EVM-KZ und AVM
      *
      *   Rev 2.0   Jan 21 2016 11:11:12   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3055                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3055                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01400!21.01.16!2.1 ! NEU                       ! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3055
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3055.
           05  PI-DRKI3055-PROGRAMM               PIC  X(008).
           05  PI-DRKI3055-UFKT-CODE              PIC  9(002).
           05  PI-DRKI3055-KEY.
               10   PI-DRKI3055-VERTRAGSID        PIC X(13).
               10   PI-DRKI3055-LFDNR-VON         PIC S9(4) COMP.
               10   PI-DRKI3055-LFDNR-BIS         PIC S9(4) COMP.
               10   PI-DRKI3055-PRODUKTID-VON     PIC S9(4) COMP.
               10   PI-DRKI3055-PRODUKTID-BIS     PIC S9(4) COMP.

       01  PO-DRKI3055.
           05  PO-DRKI3055-RC                     PIC  X(003).
               88 PO-DRKI3055-RC-OK               VALUE 'OK '.
               88 PO-DRKI3055-RC-LEER             VALUE 'LER'.
               88 PO-DRKI3055-RC-EOF              VALUE 'EOF'.
      *
           05  PO-DRKI3055-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI3055-DATEN.


               10 PO-DRKI3055-PRODUKTLFDNR  PIC S9(004) COMP OCCURS 100.
               10 PO-DRKI3055-LFDNR         PIC S9(004) COMP OCCURS 100.
               10 PO-DRKI3055-EIGENVERMKZ   PIC X(001)       OCCURS 100.
               10 PO-DRKI3055-ABSCHLUSSVBNR PIC S9(009) COMP OCCURS 100.
               10 PO-DRKI3055-PROVISIONKZ   PIC X(001)       OCCURS 100.
               10 PO-DRKI3055-TABELLE       PIC X(003)       OCCURS 100.
      *
      ******* ENDE COPY-MEMBER ***************************************

