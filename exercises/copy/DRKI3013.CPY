      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3013.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3013.CPv  $
      *
      *   Rev 2.4   Oct 01 2018 09:38:02   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.3   Sep 27 2018 16:59:12   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.2   Oct 19 2015 13:34:32   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP
      *W H O
      *
      *   Rev 2.1   Jun 17 2015 10:02:20   t13ex27
      *FKI01501: EVM-KZ anpassen bei KdBv
      *
      *   Rev 2.0   Jun 15 2015 11:59:59 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3013                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3013                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.09.2018 / 10:06:07 / T13AE52
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
      *    FÜR DIE FUNKTION DRKI3013
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3013.
           05  PI-DRKI3013-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3013-KKALFDNR   PIC S9(4) COMP.
             10 PI-DRKI3013-KKAPRODUKTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3013-KKAVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3013-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3013-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3013-KKAABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI3013-KKAEIGENVERMKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3013-NI1.
           05 PI-DRKI3013-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3013-NI7.
           05 PI-DRKI3013-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3013.
           05  PO-DRKI3013-RC                     PIC  X(003).
               88 PO-DRKI3013-RC-OK               VALUE 'OK '.
               88 PO-DRKI3013-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


