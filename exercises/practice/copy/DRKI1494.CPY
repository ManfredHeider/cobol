      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1494.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1494.CPv  $
      *
      *   Rev 2.3   Oct 26 2015 16:59:06   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP
      *
      *   Rev 2.2   Oct 19 2015 13:35:18   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP
      *W H O
      *
      *   Rev 2.1   Jan 27 2003 11:01:12   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.0   Oct 16 2002 08:33:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1494                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1494                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.10.2015 / 13:16:41 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI1494
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1494.
           05  PI-DRKI1494-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1494-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1494-GHPVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI1494-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI1494-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI1494-GHPEIGENVERMITTELT   PIC  X(0001).
END   *


      *
       01  PO-DRKI1494.
           05  PO-DRKI1494-RC                     PIC  X(003).
               88 PO-DRKI1494-RC-OK               VALUE 'OK '.
               88 PO-DRKI1494-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


