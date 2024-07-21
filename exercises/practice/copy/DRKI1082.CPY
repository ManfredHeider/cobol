      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1082.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1082.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:06:10   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1082                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1082                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.15
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
      *    FšR DIE FUNKTION DRKI1082
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1082.
           05  PI-DRKI1082-KEY.


             10 PI-DRKI1082-KOMMLAUFENDENUMMER   PIC S9(9) COMP.
             10 PI-DRKI1082-KOMMPERSONENID   PIC X(13).
             10 PI-DRKI1082-KOMMTELARTID   PIC S9(4) COMP.
             

           05  PI-DRKI1082-DATEN.


             10 PI-DRKI1082-KOMMNUMMER.
                15 PI-DRKI1082-KOMMNUMMER-LEN   PIC S9(4) COMP.
                15 PI-DRKI1082-KOMMNUMMER-TEXT   PIC X(128).
                
             10 PI-DRKI1082-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1082.
           05  PO-DRKI1082-RC                 PIC  X(003).
               88 PO-DRKI1082-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1082-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1082-DATEN.
               10 PO-DRKI1082-PTRTABLENAME.                                     
                  49 PO-DRKI1082-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1082-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1082-PTRKEYDATA.                                       
                  49 PO-DRKI1082-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1082-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1082-PTRSQLDATA.                                       
                  49 PO-DRKI1082-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1082-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
