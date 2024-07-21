      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1072.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1072.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:50   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1072                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1072                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.29
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
      *    FšR DIE FUNKTION DRKI1072
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1072.
           05  PI-DRKI1072-KEY.


             10 PI-DRKI1072-ZWPERSONENID   PIC X(13).
             10 PI-DRKI1072-ZWPERSONENROLLE   PIC X(01).
             10 PI-DRKI1072-ZWZWTEXTID   PIC S9(4) COMP.
             

           05  PI-DRKI1072-DATEN.


             10 PI-DRKI1072-LETZTEAENDERUNG   PIC X(26).
             10 PI-DRKI1072-ZWPRIORITAET   PIC S9(4) COMP.
             
      *
       01  PO-DRKI1072.
           05  PO-DRKI1072-RC                 PIC  X(003).
               88 PO-DRKI1072-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1072-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1072-DATEN.
               10 PO-DRKI1072-PTRTABLENAME.                                     
                  49 PO-DRKI1072-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1072-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1072-PTRKEYDATA.                                       
                  49 PO-DRKI1072-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1072-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1072-PTRSQLDATA.                                       
                  49 PO-DRKI1072-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1072-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
