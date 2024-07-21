      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3536.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3536.CPv  $
      *
      *   Rev 2.3   Feb 19 2019 06:47:00   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.2   Feb 18 2019 16:31:00   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.1   Jan 30 2019 11:20:28   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.0   Dec 6 2018 15:46:44 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3536                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3536                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.12.2018 / 15:46:44 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3536
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3536.
           05  PI-DRKI3536-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3536-G48VVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI3536.
           05  PO-DRKI3536-RC                     PIC  X(003).
               88 PO-DRKI3536-RC-OK               VALUE 'OK '.
               88 PO-DRKI3536-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************



