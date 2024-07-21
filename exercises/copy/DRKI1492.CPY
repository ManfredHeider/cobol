      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1492.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1492.CPv  $
      *
      *   Rev 2.3   Jan 21 2021 17:04:48   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.2   Jan 20 2021 14:08:22   u130685
      *AKI01821: Guthabenzahlung COPY
      *
      *   Rev 2.0   Oct 16 2002 08:33:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1492                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1492                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2021 / 16:59:06 / U130685
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
      *    FÜR DIE FUNKTION DRKI1492
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1492.
           05  PI-DRKI1492-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1492-FOZFONDID   PIC S9(4) COMP.
             10 PI-DRKI1492-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1492-FOZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1492-FOZVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI1492.
           05  PO-DRKI1492-RC                     PIC  X(003).
               88 PO-DRKI1492-RC-OK               VALUE 'OK '.
               88 PO-DRKI1492-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
