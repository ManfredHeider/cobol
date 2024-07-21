      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1073.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1073.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:52   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1073                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1073                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.31
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
      *    FšR DIE FUNKTION DRKI1073
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1073.
           05  PI-DRKI1073-KEY.


             10 PI-DRKI1073-ZWTEXTID   PIC S9(4) COMP.
             

           05  PI-DRKI1073-DATEN.


             10 PI-DRKI1073-ZWGUELTIGKZ   PIC X(01).
             10 PI-DRKI1073-ZWTEXT.
                15 PI-DRKI1073-ZWTEXT-LEN   PIC S9(4) COMP.
                15 PI-DRKI1073-ZWTEXT-TEXT   PIC X(100).
                
             
      *
       01  PO-DRKI1073.
           05  PO-DRKI1073-RC                 PIC  X(003).
               88 PO-DRKI1073-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1073-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1073-DATEN.
               10 PO-DRKI1073-PTRTABLENAME.                                     
                  49 PO-DRKI1073-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1073-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1073-PTRKEYDATA.                                       
                  49 PO-DRKI1073-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1073-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1073-PTRSQLDATA.                                       
                  49 PO-DRKI1073-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1073-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
