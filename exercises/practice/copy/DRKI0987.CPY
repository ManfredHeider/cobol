      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0987.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0987.CPv  $
      *
      *   Rev 2.1   Mar 03 2000 14:44:08   T13EX40
      *AKI00016 - EA-Verfahren
      *
      *   Rev 2.0   24 Jan 2000 16:35:42   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0987                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0987                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  07.02.00/18.38.05
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
      *    FšR DIE FUNKTION DRKI0987
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0987.
           05  PI-DRKI0987-KEY.


             10 PI-DRKI0987-PTRCREATORID   PIC S9(9) COMP.
             10 PI-DRKI0987-PTRID   PIC S9(9) COMP.
             
      *
       01  PO-DRKI0987.
           05  PO-DRKI0987-RC                 PIC  X(003).
               88 PO-DRKI0987-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0987-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0987-DATEN.


             10 PO-DRKI0987-PTRDESTINATION   PIC S9(9) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
