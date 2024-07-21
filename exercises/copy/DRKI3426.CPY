      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3426.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3426.CPv  $
      *
      *   Rev 2.5   May 10 2019 12:54:02   t13ae52
      *AKI01680: G42: Erweiterung Datenspiegel
      *
      *   Rev 2.4   Nov 22 2018 09:59:54   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.3   Nov 05 2018 15:47:04   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.2   Oct 27 2018 15:15:10   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.1   Oct 25 2018 14:36:18   t13ex78
      *AKI01615: OZPs für G42_DEPOTSTAMM
      *
      *   Rev 2.0   Sep 7 2018 11:04:23 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3426                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3426                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.04.2019 / 10:59:35 / T13AE52
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
      *    FÜR DIE FUNKTION DRKI3426
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3426.
           05  PI-DRKI3426-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3426-G42DVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3426.
           05  PO-DRKI3426-RC                     PIC  X(003).
               88 PO-DRKI3426-RC-OK               VALUE 'OK '.
               88 PO-DRKI3426-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3426-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3426-G42DANLAGEZIEL   PIC  X(0001).
             10 PO-DRKI3426-G42DBIC   PIC  X(0011).
             10 PO-DRKI3426-G42DDEPOTART   PIC  X(0001).
             10 PO-DRKI3426-G42DIBAN   PIC  X(0034).
             10 PO-DRKI3426-G42DKONTOINHABER   PIC  X(0030).
             10 PO-DRKI3426-G42DMANDREF   PIC  X(0035).
             10 PO-DRKI3426-G42DNRGWBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3426-G42DNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3426-G42DNRGWOBBAVH   PIC S9(6)V9(2) COMP-3.
             10 PO-DRKI3426-G42DNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3426-G42DOEFFNUNGSDAT   PIC  X(0008).
             10 PO-DRKI3426-G42DONLINEKZ   PIC  X(0001).
             10 PO-DRKI3426-G42DRGWUDEPOT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3426-G42DRISIKOKLASSE   PIC  X(0001).
             10 PO-DRKI3426-G42DSTATUS   PIC  X(0001).
             10 PO-DRKI3426-G42DWPHGAKTUDAT   PIC  X(0008).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

