      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1067.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1067.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:38   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1067                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1067                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.22
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
      *    FšR DIE FUNKTION DRKI1067
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1067.
           05  PI-DRKI1067-KEY.


             10 PI-DRKI1067-NOTIZID   PIC X(13).
             

           05  PI-DRKI1067-DATEN.


             10 PI-DRKI1067-LETZTEAENDERUNG   PIC X(26).
             10 PI-DRKI1067-NOTBEARBEITETAM   PIC X(10).
             10 PI-DRKI1067-NOTBEARBEITETVON   PIC X(30).
             10 PI-DRKI1067-NOTIZSUBTYPE   PIC X(01).
             10 PI-DRKI1067-NOTIZTEXT.
                15 PI-DRKI1067-NOTIZTEXT-LEN   PIC S9(4) COMP.
                15 PI-DRKI1067-NOTIZTEXT-TEXT   PIC X(880).
                
             10 PI-DRKI1067-NOTIZTITEL.
                15 PI-DRKI1067-NOTIZTITEL-LEN   PIC S9(4) COMP.
                15 PI-DRKI1067-NOTIZTITEL-TEXT   PIC X(50).
                
             
      *
       01  PO-DRKI1067.
           05  PO-DRKI1067-RC                 PIC  X(003).
               88 PO-DRKI1067-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1067-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1067-DATEN.
               10 PO-DRKI1067-PTRTABLENAME.                                     
                  49 PO-DRKI1067-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1067-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1067-PTRKEYDATA.                                       
                  49 PO-DRKI1067-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1067-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1067-PTRSQLDATA.                                       
                  49 PO-DRKI1067-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1067-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
