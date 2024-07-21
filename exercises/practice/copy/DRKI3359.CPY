      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3359.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3359.CPv  $
      *
      *   Rev 2.3   Feb 13 2020 06:40:16   t13ex39
      *AKI01765: Deutsche Bank - Ergänzung fehlender Kreditkartenar
      *ten
      *
      *   Rev 2.2   Sep 13 2018 09:57:26   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.1   Sep 10 2018 09:48:56   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.0   Jul 6 2018 09:30:10 T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3359                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3359                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.02.2020 / 14:47:37 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3359
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3359.
           05  PI-DRKI3359-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3359-G48KKAART   PIC  X(0003).
             10 PI-DRKI3359-G48KKANR   PIC  X(0016).
             10 PI-DRKI3359-G48KKAVNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3359.
           05  PO-DRKI3359-RC                     PIC  X(003).
               88 PO-DRKI3359-RC-OK               VALUE 'OK '.
               88 PO-DRKI3359-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3359-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3359-G48KKAABRECHBLZ   PIC  X(0008).
             10 PO-DRKI3359-G48KKAABRECHKTO   PIC  X(0010).
             10 PO-DRKI3359-G48KKABIC   PIC  X(0011).
             10 PO-DRKI3359-G48KKADELKZ   PIC  X(0001).
             10 PO-DRKI3359-G48KKAFILNRKTO   PIC  X(0003).
             10 PO-DRKI3359-G48KKAHAUPTNR   PIC  X(0016).
             10 PO-DRKI3359-G48KKAIBAN   PIC  X(0034).
             10 PO-DRKI3359-G48KKALIMIT   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3359-G48KKAMAHNSTATUS   PIC  X(0001).
             10 PO-DRKI3359-G48KKAMANDREF   PIC  X(0035).
             10 PO-DRKI3359-G48KKANACHNAMEBZ   PIC  X(0028).
             10 PO-DRKI3359-G48KKAOEFFDAT   PIC  X(0008).
             10 PO-DRKI3359-G48KKAPIN   PIC  X(0009).
             10 PO-DRKI3359-G48KKASCHLDAT   PIC  X(0008).
             10 PO-DRKI3359-G48KKAVNRHAUPT   PIC  X(0010).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

