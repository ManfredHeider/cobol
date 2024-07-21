      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0988.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0988.CPv  $
      *
      *   Rev 2.1   Mar 03 2000 14:44:12   T13EX40
      *AKI00016 - EA-Verfahren
      *
      *   Rev 2.0   24 Jan 2000 16:35:44   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0988                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0988                    *
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
      *    FšR DIE FUNKTION DRKI0988
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0988.
           05  PI-DRKI0988-KEY.


             10 PI-DRKI0988-PTRCREATORID   PIC S9(9) COMP.
             10 PI-DRKI0988-PTRID   PIC S9(9) COMP.
             

           05  PI-DRKI0988-DATEN.


             10 PI-DRKI0988-PTRDESTINATION   PIC S9(9) COMP.
             10 PI-DRKI0988-PTRITEMID   PIC S9(9) COMP.
             10 PI-DRKI0988-PTRITEMSUBTYPE   PIC X(02).
             10 PI-DRKI0988-PTRSTATE   PIC X(02).
             10 PI-DRKI0988-PTRSUBTYPE   PIC X(02).
             
      *
       01  PO-DRKI0988.
           05  PO-DRKI0988-RC                 PIC  X(003).
               88 PO-DRKI0988-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0988-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
