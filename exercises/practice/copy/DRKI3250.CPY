      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3250.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3250.CPv  $
      *
      *   Rev 2.1   Jun 07 2018 10:13:04   t13ae31
      *AKI01567: G10 Umstellung KSDS in DB2 OZP Kunde
      *
      *   Rev 2.0   Jan 30 2018 11:20:29 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3250                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3250                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.01.2018 / 11:56:40 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3250
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3250.
           05  PI-DRKI3250-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3250-G10KDROLLE   PIC S9(4) COMP.
             10 PI-DRKI3250-G10KDSPARTE   PIC  X(0003).
             10 PI-DRKI3250-G10KDVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3250.
           05  PO-DRKI3250-RC                     PIC  X(003).
               88 PO-DRKI3250-RC-OK               VALUE 'OK '.
               88 PO-DRKI3250-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3250-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3250-G10KDAENDKZ   PIC S9(4) COMP.
             10 PO-DRKI3250-G10KDANREDEKZ   PIC  X(0002).
             10 PO-DRKI3250-G10KDBIC   PIC  X(0011).
             10 PO-DRKI3250-G10KDGEBDAT   PIC  X(0008).
             10 PO-DRKI3250-G10KDGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3250-G10KDIBAN   PIC  X(0034).
             10 PO-DRKI3250-G10KDLKZ   PIC  X(0003).
             10 PO-DRKI3250-G10KDNACHNAME   PIC  X(0040).
             10 PO-DRKI3250-G10KDORT   PIC  X(0040).
             10 PO-DRKI3250-G10KDPLZ   PIC  X(0009).
             10 PO-DRKI3250-G10KDPOSTFACH   PIC  X(0045).
             10 PO-DRKI3250-G10KDSTRASSE   PIC  X(0045).
             10 PO-DRKI3250-G10KDVORNAME   PIC  X(0030).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

