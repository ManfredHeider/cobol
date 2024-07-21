      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2979.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2979.CPv  $
      *
      *   Rev 2.3   Jan 25 2018 06:53:56   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.1   Feb 18 2015 16:56:26   t13ex29
      *AKI01348: Auslagern EC-Karten in eig. Tabelle- Tabelle
      *
      *   Rev 2.0   Feb 11 2015 15:25:36 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2979                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2979                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.12.2017 / 06:54:00 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI2979
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2979.
           05  PI-DRKI2979-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2979-BECKONTOUNR   PIC S9(4) COMP.
             10 PI-DRKI2979-BECLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2979-BECVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2979.
           05  PO-DRKI2979-RC                     PIC  X(003).
               88 PO-DRKI2979-RC-OK               VALUE 'OK '.
               88 PO-DRKI2979-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI2979-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2979-BECANZKARTEN   PIC S9(4) COMP.
             10 PO-DRKI2979-BECKARTENART   PIC  X(0003).
             10 PO-DRKI2979-BECKARTENNR.
                49 PO-DRKI2979-BECKARTENNR-LEN PIC S9(4) COMP.
                49 PO-DRKI2979-BECKARTENNR-TEXT PIC X(16).
             10 PO-DRKI2979-BECPIN   PIC S9(9) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

