      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1062.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1062.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:26   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1062                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1062                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.08
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
      *    FšR DIE FUNKTION DRKI1062
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1062.
           05  PI-DRKI1062-KEY.


             10 PI-DRKI1062-HOBBYLFDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI1062-HOBBYPERSONENID   PIC X(13).
             

           05  PI-DRKI1062-DATEN.


             10 PI-DRKI1062-HOBBYTEXT   PIC X(30).
             10 PI-DRKI1062-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1062.
           05  PO-DRKI1062-RC                 PIC  X(003).
               88 PO-DRKI1062-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1062-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1062-DATEN.
               10 PO-DRKI1062-PTRTABLENAME.                                     
                  49 PO-DRKI1062-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1062-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1062-PTRKEYDATA.                                       
                  49 PO-DRKI1062-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1062-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1062-PTRSQLDATA.                                       
                  49 PO-DRKI1062-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1062-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
