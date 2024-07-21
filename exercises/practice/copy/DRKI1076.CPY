      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1076.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1076.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:58   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1076                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1076                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.33
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
      *    FšR DIE FUNKTION DRKI1076
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1076.
           05  PI-DRKI1076-KEY.


             10 PI-DRKI1076-VEREINLFDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1076-VEREINPERSONENID   PIC X(13).
             

           05  PI-DRKI1076-DATEN.


             10 PI-DRKI1076-LETZTEAENDERUNG   PIC X(26).
             10 PI-DRKI1076-VEREINFUNKTION   PIC X(30).
             10 PI-DRKI1076-VEREINTEXT   PIC X(30).
             
      *
       01  PO-DRKI1076.
           05  PO-DRKI1076-RC                 PIC  X(003).
               88 PO-DRKI1076-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1076-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1076-DATEN.
               10 PO-DRKI1076-PTRTABLENAME.                                     
                  49 PO-DRKI1076-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1076-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1076-PTRKEYDATA.                                       
                  49 PO-DRKI1076-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1076-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1076-PTRSQLDATA.                                       
                  49 PO-DRKI1076-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1076-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
