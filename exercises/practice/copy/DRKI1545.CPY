      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1545.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1545.CPv  $
      *
      *   Rev 2.1   Jul 10 2003 13:31:52   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.0   Jan 13 2003 15:52:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*


      *  UPDATE PERFORMANCE

      *****************************************************************
      *    COPY-MEMBER: DRKI1545                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1545                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  07.05.2003/15:54:44
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
      *    FÜR DIE FUNKTION DRKI1545
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1545.
           05  PI-DRKI1545-KEY.


             10 PI-DRKI1545-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1545-GHPVERTRAGSID   PIC X(13).


           05  PI-DRKI1545-DATEN.


             10 PI-DRKI1545-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI1545-GHPPERFLFDJAHR  PIC S9(13)V9(5) COMP-3.


      *
       01  PO-DRKI1545.
           05  PO-DRKI1545-RC                 PIC  X(003).
               88 PO-DRKI1545-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1545-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
