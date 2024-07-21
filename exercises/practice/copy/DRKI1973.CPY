      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1973.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1973.CPv  $
      *
      *   Rev 2.7   Mar 30 2020 08:46:00   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.0   Mar 4 2020 13:26:53 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1973                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1973                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.03.2020 / 13:26:53 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI1973
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1973.
           05  PI-DRKI1973-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1973-VBBVERSBEDINGLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1973-VBBVERSBEDINGUNGID   PIC S9(9) COMP.
             10 PI-DRKI1973-VBBVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1973-VBBVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI1973.
           05  PO-DRKI1973-RC                     PIC  X(003).
               88 PO-DRKI1973-RC-OK               VALUE 'OK '.
               88 PO-DRKI1973-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1973-NI.
               10 NI-DRKI1973-TAB.
                  15 DRKI1973-ELE
                     PIC S9(04) COMP OCCURS 4 TIMES.
               10 NI-DRKI1973 REDEFINES NI-DRKI1973-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI1973-NI-VBBAVB-KNR   PIC S9(4) COMP.
             15 PO-DRKI1973-NI-VBBAVB-KTY   PIC S9(4) COMP.
             15 PO-DRKI1973-N-VBBBDNGNG-KNR   PIC S9(4) COMP.
             15 PO-DRKI1973-N-VBBBDNGNG-KTY   PIC S9(4) COMP.
END   *

           05  PO-DRKI1973-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI1973-VBBAVB-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI1973-VBBAVB-KTY   PIC S9(4) COMP.
             10 PO-DRKI1973-VBBBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI1973-VBBBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PO-DRKI1973-VBBVEBPARTNERGES   PIC S9(4) COMP.
             10 PO-DRKI1973-VBBVERSBEDINGKEY   PIC  X(0002).
             10 PO-DRKI1973-VBBVERSBEDINGUNG.
                49 PO-DRKI1973-VBBVRSBDNGNG-LEN PIC S9(4) COMP.
                49 PO-DRKI1973-VBBVRSBDNGNG-TEXT PIC X(50).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

