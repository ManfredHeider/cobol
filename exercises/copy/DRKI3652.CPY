      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.04.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3652.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3652.CPv  $
      *
      *   Rev 2.2   Jun 25 2019 09:14:46   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.1   Jun 04 2019 14:45:46   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.0   Apr 16 2019 15:24:27 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3652                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3652                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.04.2019 / 15:24:27 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3652
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3652.
           05  PI-DRKI3652-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3652-DBFVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3652.
           05  PO-DRKI3652-RC                     PIC  X(003).
               88 PO-DRKI3652-RC-OK               VALUE 'OK '.
               88 PO-DRKI3652-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3652-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3652-DBFFILART-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3652-DBFFILART-KTY   PIC S9(4) COMP.
             10 PO-DRKI3652-DBFFILNAME-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3652-DBFFILNAME-KTY   PIC S9(4) COMP.
             10 PO-DRKI3652-DBFFILORT-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3652-DBFFILORT-KTY   PIC S9(4) COMP.
             10 PO-DRKI3652-DBFFILSTR-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI3652-DBFFILSTR-KTY   PIC S9(4) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


