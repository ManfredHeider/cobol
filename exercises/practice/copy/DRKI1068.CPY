      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1068.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:40   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:20:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1068                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1068                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.12.23
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
      *    FšR DIE FUNKTION DRKI1068
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1068.
           05  PI-DRKI1068-KEY.


             10 PI-DRKI1068-PERSONENID   PIC X(13).
             

           05  PI-DRKI1068-DATEN.


             10 PI-DRKI1068-LETZTEAENDERUNG   PIC X(26).
             10 PI-DRKI1068-PERSONANREDEID   PIC S9(4) COMP.
             10 PI-DRKI1068-NI-PERSONANREDEID   PIC S9(4) COMP.
             10 PI-DRKI1068-PERSONANZNAME   PIC X(30).
             10 PI-DRKI1068-PERSONANZVORNAME   PIC X(15).
             10 PI-DRKI1068-PERSONEMPFOHLVON.
                15 PI-DRKI1068-PRSNMPFHLVN-LN   PIC S9(4) COMP.
                15 PI-DRKI1068-PRSNMPFHLVN-TXT   PIC X(50).
                
             10 PI-DRKI1068-PERSONGEBGRUENDDAT   PIC X(10).
             10 PI-DRKI1068-PERSONKUNDENART   PIC X(01).
             10 PI-DRKI1068-PERSONNAMEPHONCODE   PIC X(15).
             10 PI-DRKI1068-PERSONNOTIZID   PIC X(13).
             10 PI-DRKI1068-PERSONSTEUERBERAT.
                15 PI-DRKI1068-PRSNSTRBRT-LN   PIC S9(4) COMP.
                15 PI-DRKI1068-PRSNSTRBRT-TXT   PIC X(50).
                
             10 PI-DRKI1068-PERSONSTEUERBTELEF.
                15 PI-DRKI1068-PRSNSTRBTLF-LN   PIC S9(4) COMP.
                15 PI-DRKI1068-PRSNSTRBTLF-TXT   PIC X(20).
                
             10 PI-DRKI1068-PERSONSUCHNAME   PIC X(15).
             10 PI-DRKI1068-PERSONSUCHVORNAME   PIC X(15).
             
      *
       01  PO-DRKI1068.
           05  PO-DRKI1068-RC                 PIC  X(003).
               88 PO-DRKI1068-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1068-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1068-DATEN.
               10 PO-DRKI1068-PTRTABLENAME.                                     
                  49 PO-DRKI1068-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1068-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1068-PTRKEYDATA.                                       
                  49 PO-DRKI1068-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1068-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1068-PTRSQLDATA.                                       
                  49 PO-DRKI1068-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1068-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
