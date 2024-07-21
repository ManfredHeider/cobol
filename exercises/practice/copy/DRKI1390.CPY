      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1390.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1390.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 13:40:28   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Apr 09 2002 08:32:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1390                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1390                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  17.04.2002/12:32:03
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
      *    FšR DIE FUNKTION DRKI1390
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1390.
           05  PI-DRKI1390-KEY.


             10 PI-DRKI1390-EVQ-ID   PIC X(12).

      *
       01  PO-DRKI1390.
           05  PO-DRKI1390-RC                 PIC  X(003).
               88 PO-DRKI1390-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1390-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1390-DATEN.


             10 PO-DRKI1390-EVQ-EVENT-ID   PIC X(12).
             10 PO-DRKI1390-EVQ-EVENTUSER   PIC X(30).
             10 PO-DRKI1390-EVQ-LAST-TRY   PIC X(26).
             10 PO-DRKI1390-EVQ-PRIORITY   PIC S9(9) COMP.
             10 PO-DRKI1390-EVQ-PROCESS-STATUS   PIC S9(9) COMP.
             10 PO-DRKI1390-EVQ-REG-EVENT-ID   PIC S9(9) COMP.
             10 PO-DRKI1390-EVQ-REG-SERVICE-ID   PIC S9(9) COMP.
             10 PO-DRKI1390-EVQ-TRIGGER-TIME   PIC X(26).


      *
      ******* ENDE COPY-MEMBER **************************************
