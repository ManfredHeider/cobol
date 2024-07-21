      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0982.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0982.CPv  $
      *
      *   Rev 2.1   Mar 03 2000 14:43:54   T13EX40
      *AKI00016 - EA-Verfahren
      *
      *   Rev 2.0   24 Jan 2000 16:35:34   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0982                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0982                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  07.02.00/18.38.30
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
      *    FšR DIE FUNKTION DRKI0982
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0982.
           05  PI-DRKI0982-KEY.
             10 PI-DRKI0982-PTRCREATORID   PIC S9(9) COMP.
             10 PI-DRKI0982-PTRID   PIC S9(9) COMP.
             10 PI-DRKI0982-PTRSUBTYPE   PIC X(02).
             
      *
       01  PO-DRKI0982.
           05  PO-DRKI0982-RC                 PIC  X(003).
               88 PO-DRKI0982-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0982-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0982-DATEN.


             10 PO-DRKI0982-PTRSENDERID   PIC S9(9) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
