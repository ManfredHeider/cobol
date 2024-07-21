      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1075.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1075.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:56   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1075                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1075                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.03.00/18.51.02
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
      *    FšR DIE FUNKTION DRKI1075
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1075.
           05  PI-DRKI1075-KEY.


             10 PI-DRKI1075-ADMAPERSONENID   PIC X(13).
             10 PI-DRKI1075-ADMAPERSONENROLLE   PIC X(01).
             

           05  PI-DRKI1075-DATEN.


             10 PI-DRKI1075-ADMAARBSTATIONID   PIC S9(9) COMP.
             10 PI-DRKI1075-N-DMRBSTTND   PIC S9(4) COMP.
             10 PI-DRKI1075-ADMAAUSTRITTSDATUM   PIC X(10).
             10 PI-DRKI1075-ADMADIREKTION   PIC S9(4) COMP.
             10 PI-DRKI1075-NI-ADMADIREKTION   PIC S9(4) COMP.
             10 PI-DRKI1075-ADMAINPROBEKZ   PIC X(01).
             10 PI-DRKI1075-ADMAPRAXISPROVSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1075-N-DMPRXSPRVSTZ   PIC S9(4) COMP.
             10 PI-DRKI1075-ADMARUECKSTUFDATUM   PIC X(10).
             10 PI-DRKI1075-ADMASPERRE   PIC X(01).
             10 PI-DRKI1075-ADMASTRUKTURSTUFE   PIC S9(9) COMP.
             10 PI-DRKI1075-N-DMSTRKTRSTF   PIC S9(4) COMP.
             10 PI-DRKI1075-ADMAUMSTUFUNGSDAT   PIC X(10).
             
      *
       01  PO-DRKI1075.
           05  PO-DRKI1075-RC                 PIC  X(003).
               88 PO-DRKI1075-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1075-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1075-DATEN.
               10 PO-DRKI1075-PTRTABLENAME.                                     
                  49 PO-DRKI1075-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1075-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1075-PTRKEYDATA.                                       
                  49 PO-DRKI1075-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1075-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1075-PTRSQLDATA.                                       
                  49 PO-DRKI1075-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1075-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
