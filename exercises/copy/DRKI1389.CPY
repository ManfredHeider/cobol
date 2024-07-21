      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1389.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1389.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 13:39:22   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Apr 09 2002 08:32:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1389                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1389                    *
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
      *    FšR DIE FUNKTION DRKI1389
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1389.
           05  PI-DRKI1389-KEY.


             10 PI-DRKI1389-EVQ-ID   PIC X(12).


           05  PI-DRKI1389-DATEN.


             10 PI-DRKI1389-EVQ-EVENT-ID   PIC X(12).
             10 PI-DRKI1389-EVQ-EVENTUSER   PIC X(30).
             10 PI-DRKI1389-EVQ-LAST-TRY   PIC X(26).
             10 PI-DRKI1389-EVQ-PRIORITY   PIC S9(9) COMP.
             10 PI-DRKI1389-EVQ-PROCESS-STATUS   PIC S9(9) COMP.
             10 PI-DRKI1389-EVQ-REG-EVENT-ID   PIC S9(9) COMP.
             10 PI-DRKI1389-EVQ-REG-SERVICE-ID   PIC S9(9) COMP.
             10 PI-DRKI1389-EVQ-TRIGGER-TIME   PIC X(26).

      *
       01  PO-DRKI1389.
           05  PO-DRKI1389-RC                 PIC  X(003).
               88 PO-DRKI1389-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1389-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
