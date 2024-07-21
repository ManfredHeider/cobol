      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1064.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1064.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:32   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1064                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1064                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.11
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
      *    FšR DIE FUNKTION DRKI1064
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1064.
           05  PI-DRKI1064-KEY.


             10 PI-DRKI1064-KBWLAUFENDENUMMER   PIC S9(9) COMP.
             10 PI-DRKI1064-KBWPERSONENID   PIC X(13).
             10 PI-DRKI1064-KBWPERSONENROLLE   PIC X(01).
             

           05  PI-DRKI1064-DATEN.


             10 PI-DRKI1064-KBWEIGENTUMSARTID   PIC X(3).
             10 PI-DRKI1064-KBWERWMITTEL   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1064-NI-KBWERWMITTEL   PIC S9(4) COMP.
             10 PI-DRKI1064-KBWERWMITTELIMJ   PIC X(10).
             10 PI-DRKI1064-KBWERWMITTELWKZ   PIC X(01).
             10 PI-DRKI1064-KBWGRUNDSTVORH   PIC X(01).
             10 PI-DRKI1064-KBWGRUNDSTWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1064-NI-KBWGRUNDSTWERT   PIC S9(4) COMP.
             10 PI-DRKI1064-KBWGRUNDSTWWKZ   PIC X(01).
             10 PI-DRKI1064-KBWKAUFBAUDATUM   PIC X(10).
             10 PI-DRKI1064-KBWMONBELGRSTWKZ   PIC X(01).
             10 PI-DRKI1064-KBWMONBELGRUNDST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1064-N-KBWMNBLGRNDST   PIC S9(4) COMP.
             10 PI-DRKI1064-KBWMONBELOBJECK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1064-NI-KBWMONBELOBJECK   PIC S9(4) COMP.
             10 PI-DRKI1064-KBWMONBELOBJWKZ   PIC X(01).
             10 PI-DRKI1064-KBWOBJEKTWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1064-NI-KBWOBJEKTWERT   PIC S9(4) COMP.
             10 PI-DRKI1064-KBWOBJEKTWERTWKZ   PIC X(01).
             10 PI-DRKI1064-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1064.
           05  PO-DRKI1064-RC                 PIC  X(003).
               88 PO-DRKI1064-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1064-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1064-DATEN.
               10 PO-DRKI1064-PTRTABLENAME.                                     
                  49 PO-DRKI1064-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1064-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1064-PTRKEYDATA.                                       
                  49 PO-DRKI1064-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1064-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1064-PTRSQLDATA.                                       
                  49 PO-DRKI1064-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1064-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
