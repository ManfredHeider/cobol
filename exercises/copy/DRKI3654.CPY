      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.04.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3654.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3654.CPv  $
      *
      *   Rev 2.2   Jun 25 2019 09:15:22   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.1   Jun 04 2019 14:46:14   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.0   Apr 16 2019 15:24:31 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3654                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3654                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.04.2019 / 15:24:31 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3654
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3654.
           05  PI-DRKI3654-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3654-DBFVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI3654-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3654-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3654-DBFFILART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3654-DBFFILART-KTY   PIC S9(4) COMP.
             10 PI-DRKI3654-DBFFILNAME-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3654-DBFFILNAME-KTY   PIC S9(4) COMP.
             10 PI-DRKI3654-DBFFILORT-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3654-DBFFILORT-KTY   PIC S9(4) COMP.
             10 PI-DRKI3654-DBFFILSTR-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3654-DBFFILSTR-KTY   PIC S9(4) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3654-NI1.
           05 PI-DRKI3654-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3654-NI7.
           05 PI-DRKI3654-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3654.
           05  PO-DRKI3654-RC                     PIC  X(003).
               88 PO-DRKI3654-RC-OK               VALUE 'OK '.
               88 PO-DRKI3654-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


