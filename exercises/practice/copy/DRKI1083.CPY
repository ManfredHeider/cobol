      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1083.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1083.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:06:12   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1083                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1083                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.16
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
      *    FšR DIE FUNKTION DRKI1083
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1083.
           05  PI-DRKI1083-KEY.


             10 PI-DRKI1083-KIPERSONENID   PIC X(13).
             10 PI-DRKI1083-KIPERSONENROLLE   PIC X(01).
             

           05  PI-DRKI1083-DATEN.


             10 PI-DRKI1083-KIANZAHLBETREUER   PIC S9(4) COMP.
             10 PI-DRKI1083-N-KNZHLBTRR   PIC S9(4) COMP.
             10 PI-DRKI1083-KIINFOWUNSCH   PIC X(01).
             10 PI-DRKI1083-KIKUNDENNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1083-NI-KIKUNDENNUMMER   PIC S9(4) COMP.
             10 PI-DRKI1083-KIKUNDENSTATUSID   PIC X(01).
             10 PI-DRKI1083-KIORTDERARBSTAETT   PIC X(3).
             10 PI-DRKI1083-KIVKUNDEDUZENKZ   PIC X(01).
             10 PI-DRKI1083-KIVSABERECHDATUM   PIC X(10).
             
      *
       01  PO-DRKI1083.
           05  PO-DRKI1083-RC                 PIC  X(003).
               88 PO-DRKI1083-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1083-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1083-DATEN.
               10 PO-DRKI1083-PTRTABLENAME.                                     
                  49 PO-DRKI1083-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1083-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1083-PTRKEYDATA.                                       
                  49 PO-DRKI1083-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1083-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1083-PTRSQLDATA.                                       
                  49 PO-DRKI1083-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1083-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
