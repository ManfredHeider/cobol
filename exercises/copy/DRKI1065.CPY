      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1065.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1065.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:34   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1065                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1065                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.18
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
      *    FšR DIE FUNKTION DRKI1065
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1065.
           05  PI-DRKI1065-KEY.


             10 PI-DRKI1065-MIETELAUFENDENUM   PIC S9(9) COMP.
             10 PI-DRKI1065-MIETEPERSONENID   PIC X(13).
             10 PI-DRKI1065-MIETEPERSONENROLLE   PIC X(01).
             

           05  PI-DRKI1065-DATEN.


             10 PI-DRKI1065-LETZTEAENDERUNG   PIC X(26).
             10 PI-DRKI1065-MIETEMONKALTMIETE   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI1065-N-MTMNKLTMT   PIC S9(4) COMP.
             10 PI-DRKI1065-MIETEMONNEBKOSTEN   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI1065-N-MTMNNBKSTN   PIC S9(4) COMP.
             10 PI-DRKI1065-MIETEMONNEBKWKZ   PIC X(01).
             10 PI-DRKI1065-MIETESTEIGERUNG   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1065-NI-MIETESTEIGERUNG   PIC S9(4) COMP.
             10 PI-DRKI1065-MIETEWOHNFLAECHE   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1065-N-MTWHNFLCH   PIC S9(4) COMP.
             10 PI-DRKI1065-MIETEWOHNGZUSCH   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI1065-NI-MIETEWOHNGZUSCH   PIC S9(4) COMP.
             10 PI-DRKI1065-MIETEWOHNGZWKZ   PIC X(01).
             10 PI-DRKI1065-MIETWMONKMIETWKZ   PIC X(01).
             
      *
       01  PO-DRKI1065.
           05  PO-DRKI1065-RC                 PIC  X(003).
               88 PO-DRKI1065-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1065-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1065-DATEN.
               10 PO-DRKI1065-PTRTABLENAME.                                     
                  49 PO-DRKI1065-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1065-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1065-PTRKEYDATA.                                       
                  49 PO-DRKI1065-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1065-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1065-PTRSQLDATA.                                       
                  49 PO-DRKI1065-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1065-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
