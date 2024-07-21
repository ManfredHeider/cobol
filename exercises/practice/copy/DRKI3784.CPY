      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3784.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3784.CPv  $
      *
      *   Rev 2.2   Oct 06 2020 16:38:34   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 11:59:20   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:14:56 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3784                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3784                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.05.2020 / 16:41:43 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3784
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3784.
           05  PI-DRKI3784-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3784-DCOVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3784-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3784-NI-DCOAKTSALDO      PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOAUSZAHLUNG    PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCODELETEKZ      PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOFAELLIG       PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOFINBETRAG     PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOIBANKREDIT    PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOPRODUKTART    PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOPRODUKTGEBER  PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOPRODUKTID     PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCORATE          PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCORESTLAUF      PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOSOLLZINS      PIC S9(4) COMP.
             15 PI-DRKI3784-NI-DCOZUSAGE        PIC S9(4) COMP.
END   *

           05  PI-DRKI3784-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3784-DCOAKTSALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3784-DCOAUSZAHLUNG   PIC  X(010).
             10 PI-DRKI3784-DCODELETEKZ   PIC  X(0001).
             10 PI-DRKI3784-DCOFAELLIG   PIC  X(010).
             10 PI-DRKI3784-DCOFINBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3784-DCOIBANKREDIT   PIC  X(0034).
             10 PI-DRKI3784-DCOPRODUKTART   PIC  X(0001).
             10 PI-DRKI3784-DCOPRODUKTGEBER   PIC  X(0030).
             10 PI-DRKI3784-DCOPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI3784-DCORATE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3784-DCORESTLAUF   PIC S9(3)V COMP-3.
             10 PI-DRKI3784-DCOSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3784-DCOZUSAGE   PIC  X(010).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3784-NI1.
           05 PI-DRKI3784-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 13.
       01  PI-DRKI3784-NI7.
           05 PI-DRKI3784-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 13.
      *
       01  PO-DRKI3784.
           05  PO-DRKI3784-RC                     PIC  X(003).
               88 PO-DRKI3784-RC-OK               VALUE 'OK '.
               88 PO-DRKI3784-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


