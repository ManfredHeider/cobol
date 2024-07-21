      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2982.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2982.CPv  $
      *
      *   Rev 2.3   Jan 25 2018 06:54:24   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.1   Feb 18 2015 16:56:40   t13ex29
      *AKI01348: Auslagern EC-Karten in eig. Tabelle- Tabelle
      *
      *   Rev 2.0   Feb 11 2015 15:25:42 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2982                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2982                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.12.2017 / 06:54:05 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI2982
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2982.
           05  PI-DRKI2982-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2982-BECKONTOUNR   PIC S9(4) COMP.
             10 PI-DRKI2982-BECLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2982-BECVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2982-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2982-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2982-BECANZKARTEN   PIC S9(4) COMP.
             10 PI-DRKI2982-BECKARTENART   PIC  X(0003).
             10 PI-DRKI2982-BECKARTENNR.
                49 PI-DRKI2982-BECKARTENNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2982-BECKARTENNR-TEXT PIC X(16).
             10 PI-DRKI2982-BECPIN   PIC S9(9) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2982-NI1.
           05 PI-DRKI2982-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2982-NI7.
           05 PI-DRKI2982-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2982.
           05  PO-DRKI2982-RC                     PIC  X(003).
               88 PO-DRKI2982-RC-OK               VALUE 'OK '.
               88 PO-DRKI2982-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

