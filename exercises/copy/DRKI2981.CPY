      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2981.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2981.CPv  $
      *
      *   Rev 2.3   Jan 25 2018 06:54:10   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.1   Feb 18 2015 16:56:36   t13ex29
      *AKI01348: Auslagern EC-Karten in eig. Tabelle- Tabelle
      *
      *   Rev 2.0   Feb 11 2015 15:25:40 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2981                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2981                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.12.2017 / 06:54:04 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI2981
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2981.
           05  PI-DRKI2981-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2981-BECKONTOUNR   PIC S9(4) COMP.
             10 PI-DRKI2981-BECLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2981-BECVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2981-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2981-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2981-BECANZKARTEN   PIC S9(4) COMP.
             10 PI-DRKI2981-BECKARTENART   PIC  X(0003).
             10 PI-DRKI2981-BECKARTENNR.
                49 PI-DRKI2981-BECKARTENNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2981-BECKARTENNR-TEXT PIC X(16).
             10 PI-DRKI2981-BECPIN   PIC S9(9) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2981-NI1.
           05 PI-DRKI2981-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2981-NI7.
           05 PI-DRKI2981-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI2981.
           05  PO-DRKI2981-RC                     PIC  X(003).
               88 PO-DRKI2981-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2981-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

