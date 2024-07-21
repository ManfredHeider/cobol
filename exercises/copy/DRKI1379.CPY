      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1379.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1379.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 13:37:10   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Mar 12 2002 12:46:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1379                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1379                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.04.2002/12:28:04
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1379
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1379.
           05  PI-DRKI1379-KEY.


             10 PI-DRKI1379-CON-ID   PIC S9(9) COMP.

      *
       01  PO-DRKI1379.
           05  PO-DRKI1379-RC                 PIC  X(003).
               88 PO-DRKI1379-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1379-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1379-DATEN.


             10 PO-DRKI1379-CON-CC-REG-ID   PIC S9(9) COMP.
             10 PO-DRKI1379-CON-OBJ-CLASS-ID   PIC S9(9) COMP.
             10 PO-DRKI1379-CON-OBJECT-ID   PIC X(40).
             10 PO-DRKI1379-CON-REASON   PIC X(60).
             10 PO-DRKI1379-CON-SUBTYPE   PIC S9(4) COMP.
             10 PO-DRKI1379-CON-TIMESTAMP   PIC X(26).
             10 PO-DRKI1379-CON-USER   PIC X(30).


      *
      ******* ENDE COPY-MEMBER **************************************
