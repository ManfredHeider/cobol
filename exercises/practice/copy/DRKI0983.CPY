      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0983.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0983.CPv  $
      *
      *   Rev 2.1   Mar 03 2000 14:43:56   T13EX40
      *AKI00016 - EA-Verfahren
      *
      *   Rev 2.0   24 Jan 2000 16:35:36   P13SP99
      *Neuanlage
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0983                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI0983                    *
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
      *    FšR DIE FUNKTION DRKI0983
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0983.
           05  PI-DRKI0983-KEY.


             10 PI-DRKI0983-PTRCREATORID   PIC S9(9) COMP.
             10 PI-DRKI0983-PTRID   PIC S9(9) COMP.
             10 PI-DRKI0983-PTRSUBTYPE   PIC X(02).
             

           05  PI-DRKI0983-DATEN.


             10 PI-DRKI0983-PTRCOMMITSTATE   PIC X(02).
             10 PI-DRKI0983-PTRISACCEPTED   PIC S9(4) COMP.
             10 PI-DRKI0983-PTRNETENTRY   PIC S9(9) COMP.
             10 PI-DRKI0983-PTRORIGIN   PIC S9(9) BINARY.
             10 PI-DRKI0983-PTRSENDERID   PIC S9(9) COMP.
             10 PI-DRKI0983-PTRSITEPRIORITY   PIC S9(9) COMP.
             10 PI-DRKI0983-PTRSTATE   PIC X(02).
             10 PI-DRKI0983-PTRUSERDATA.
                15 PI-DRKI0983-PTRUSERDATA-LEN   PIC S9(4) COMP.
                15 PI-DRKI0983-PTRUSERDATA-TEXT   PIC X(20).
                
             
      *
       01  PO-DRKI0983.
           05  PO-DRKI0983-RC                 PIC  X(003).
               88 PO-DRKI0983-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0983-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
