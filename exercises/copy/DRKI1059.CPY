      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1059.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1059.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:20   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:19:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1059                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1059                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.11.59
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
      *    FšR DIE FUNKTION DRKI1059
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1059.
           05  PI-DRKI1059-KEY.


             10 PI-DRKI1059-DATFWERTLFDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1059-DATFWERTPERSONENID   PIC X(13).
             10 PI-DRKI1059-DATFWERTVBID   PIC X(13).
             10 PI-DRKI1059-DATFWERTVBROLLE   PIC X(01).
             10 PI-DRKI1059-DATFWLFDNUMMER   PIC S9(9) COMP.
             

           05  PI-DRKI1059-DATEN.


             10 PI-DRKI1059-DATFWERTWERT   PIC X(60).
             10 PI-DRKI1059-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1059.
           05  PO-DRKI1059-RC                 PIC  X(003).
               88 PO-DRKI1059-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1059-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1059-DATEN.
               10 PO-DRKI1059-PTRTABLENAME.                                     
                  49 PO-DRKI1059-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1059-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1059-PTRKEYDATA.                                       
                  49 PO-DRKI1059-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1059-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1059-PTRSQLDATA.                                       
                  49 PO-DRKI1059-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1059-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
