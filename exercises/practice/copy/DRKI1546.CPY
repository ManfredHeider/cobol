      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1546.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1546.CPv  $
      *
      *   Rev 2.1   Jul 10 2003 13:31:50   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.0   Jan 13 2003 15:52:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*


      *  UPDATE EIGENVERMITTELT-KZ

      *****************************************************************
      *    COPY-MEMBER: DRKI1546                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1546                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  16.05.2003/15:19:26
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
      *    FÜR DIE FUNKTION DRKI1546
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1546.
           05  PI-DRKI1546-KEY.


             10 PI-DRKI1546-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1546-GHPVERTRAGSID   PIC X(13).


           05  PI-DRKI1546-DATEN.


             10 PI-DRKI1546-GHPEIGENVERMITTELT   PIC X(01).


      *
       01  PO-DRKI1546.
           05  PO-DRKI1546-RC                 PIC  X(003).
               88 PO-DRKI1546-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1546-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
