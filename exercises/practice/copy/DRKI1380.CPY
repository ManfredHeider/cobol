      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1380.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1380.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 13:39:40   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Apr 09 2002 08:31:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1380                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1380                    *
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
      *    FšR DIE FUNKTION DRKI1380
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1380.
           05  PI-DRKI1380-KEY.


             10 PI-DRKI1380-CON-ID   PIC S9(9) COMP.


           05  PI-DRKI1380-DATEN.


             10 PI-DRKI1380-CON-CC-REG-ID   PIC S9(9) COMP.
             10 PI-DRKI1380-CON-OBJ-CLASS-ID   PIC S9(9) COMP.
             10 PI-DRKI1380-CON-OBJECT-ID   PIC X(40).
             10 PI-DRKI1380-CON-REASON   PIC X(60).
             10 PI-DRKI1380-CON-SUBTYPE   PIC S9(4) COMP.
             10 PI-DRKI1380-CON-TIMESTAMP   PIC X(26).
             10 PI-DRKI1380-CON-USER   PIC X(30).

      *
       01  PO-DRKI1380.
           05  PO-DRKI1380-RC                 PIC  X(003).
               88 PO-DRKI1380-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1380-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
