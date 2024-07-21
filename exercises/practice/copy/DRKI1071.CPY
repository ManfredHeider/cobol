      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1071.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1071.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:46   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1071                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1071                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.28
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
      *    FšR DIE FUNKTION DRKI1071
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1071.
           05  PI-DRKI1071-KEY.


             10 PI-DRKI1071-SPBETRPERSONENID   PIC X(13).
             10 PI-DRKI1071-SPBETRPERSROLLE   PIC X(01).
             

           05  PI-DRKI1071-DATEN.


             10 PI-DRKI1071-LETZTEAENDERUNG   PIC X(26).
             10 PI-DRKI1071-SPBETRERHBARUMABJ   PIC X(10).
             10 PI-DRKI1071-SPBETRERHBUMWKZ   PIC X(01).
             10 PI-DRKI1071-SPBETRERHOEHBARUM   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1071-N-SPBTRRHHBRM   PIC S9(4) COMP.
             10 PI-DRKI1071-SPBETRMONATLICH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1071-NI-SPBETRMONATLICH   PIC S9(4) COMP.
             10 PI-DRKI1071-SPBETRMONATLICHWKZ   PIC X(01).
             10 PI-DRKI1071-SPBETRZUSGBETRWKZ   PIC X(01).
             10 PI-DRKI1071-SPBETRZUSGELBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1071-N-SPBTRZSGLBTRG   PIC S9(4) COMP.
             
      *
       01  PO-DRKI1071.
           05  PO-DRKI1071-RC                 PIC  X(003).
               88 PO-DRKI1071-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1071-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1071-DATEN.
               10 PO-DRKI1071-PTRTABLENAME.                                     
                  49 PO-DRKI1071-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1071-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1071-PTRKEYDATA.                                       
                  49 PO-DRKI1071-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1071-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1071-PTRSQLDATA.                                       
                  49 PO-DRKI1071-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1071-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
