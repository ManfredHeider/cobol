      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3783.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3783.CPv  $
      *
      *   Rev 2.2   Oct 06 2020 16:38:28   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 11:58:34   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:14:54 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3783                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3783                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.05.2020 / 16:41:42 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3783
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3783.
           05  PI-DRKI3783-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI3783-DCOVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3783-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3783-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3783-DCOAKTSALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3783-DCOAUSZAHLUNG   PIC  X(010).
             10 PI-DRKI3783-DCODELETEKZ   PIC  X(0001).
             10 PI-DRKI3783-DCOFAELLIG   PIC  X(010).
             10 PI-DRKI3783-DCOFINBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3783-DCOIBANKREDIT   PIC  X(0034).
             10 PI-DRKI3783-DCOPRODUKTART   PIC  X(0001).
             10 PI-DRKI3783-DCOPRODUKTGEBER   PIC  X(0030).
             10 PI-DRKI3783-DCOPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI3783-DCORATE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3783-DCORESTLAUF   PIC S9(3)V COMP-3.
             10 PI-DRKI3783-DCOSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3783-DCOZUSAGE   PIC  X(010).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3783-NI1.
           05 PI-DRKI3783-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3783-NI7.
           05 PI-DRKI3783-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI3783.
           05  PO-DRKI3783-RC                     PIC  X(003).
               88 PO-DRKI3783-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3783-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


