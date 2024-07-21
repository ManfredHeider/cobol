      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1078.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1078.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:06:02   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1078                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1078                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  20.03.00/10.39.19
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
      *    FšR DIE FUNKTION DRKI1078
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1078.
           05  PI-DRKI1078-KEY.


             10 PI-DRKI1078-MITARBPERSONENID   PIC X(13).
             10 PI-DRKI1078-MITARBPERSROLLE   PIC X(01).
             

           05  PI-DRKI1078-DATEN.


             10 PI-DRKI1078-MITARBEITERNR   PIC S9(9) COMP.
             10 PI-DRKI1078-NI-MITARBEITERNR   PIC S9(4) COMP.
             10 PI-DRKI1078-MITARBVORGESPERSID   PIC X(13).
             10 PI-DRKI1078-MITARBVORGESROLLE   PIC X(01).
             
      *
       01  PO-DRKI1078.
           05  PO-DRKI1078-RC                 PIC  X(003).
               88 PO-DRKI1078-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1078-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1078-DATEN.
               10 PO-DRKI1078-PTRTABLENAME.                                     
                  49 PO-DRKI1078-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1078-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1078-PTRKEYDATA.                                       
                  49 PO-DRKI1078-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1078-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1078-PTRSQLDATA.                                       
                  49 PO-DRKI1078-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1078-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
