      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1058.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1058.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:18   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:19:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1058                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1058                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.11.58
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
      *    FšR DIE FUNKTION DRKI1058
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1058.
           05  PI-DRKI1058-KEY.


             10 PI-DRKI1058-DATFDEFLFDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1058-DATFDEFVBID   PIC X(13).
             10 PI-DRKI1058-DATFDEFVBROLLE   PIC X(01).
             

           05  PI-DRKI1058-DATEN.


             10 PI-DRKI1058-DATFDEFBEZEICHNUNG   PIC X(15).
             10 PI-DRKI1058-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1058.
           05  PO-DRKI1058-RC                 PIC  X(003).
               88 PO-DRKI1058-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1058-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1058-DATEN.
               10 PO-DRKI1058-PTRTABLENAME.                                     
                  49 PO-DRKI1058-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1058-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1058-PTRKEYDATA.                                       
                  49 PO-DRKI1058-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1058-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1058-PTRSQLDATA.                                       
                  49 PO-DRKI1058-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1058-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
