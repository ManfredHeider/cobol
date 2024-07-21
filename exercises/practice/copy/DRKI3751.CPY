      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.01.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3751.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3751.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 12:39:42   t13ex78
      *AKI01731: Standard OZP G66_KUNDE
      *
      *   Rev 2.0   Jan 09 2020 10:16:32   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3751                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3751                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2020 / 11:44:51 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3751
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3751.
           05  PI-DRKI3751-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3751-G66KLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3751-G66KVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3751.
           05  PO-DRKI3751-RC                     PIC  X(003).
               88 PO-DRKI3751-RC-OK               VALUE 'OK '.
               88 PO-DRKI3751-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3751-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3751-G66KANREDE   PIC  X(0001).
             10 PO-DRKI3751-G66KGEBURTSDATUM   PIC  X(0008).
             10 PO-DRKI3751-G66KISOCODE   PIC  X(0003).
             10 PO-DRKI3751-G66KNAME   PIC  X(0035).
             10 PO-DRKI3751-G66KORT   PIC  X(0030).
             10 PO-DRKI3751-G66KPLZ   PIC  X(0009).
             10 PO-DRKI3751-G66KSTRHNR   PIC  X(0040).
             10 PO-DRKI3751-G66KVORNAME   PIC  X(0035).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

