      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1057.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1057.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 13:05:16   T13EX40
      *AKI00016 - doppelbetreuung Person
      *
      *   Rev 2.0   Feb 09 2000 15:19:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1057                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1057                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.03.00/17.11.56
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
      *    FšR DIE FUNKTION DRKI1057
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1057.
           05  PI-DRKI1057-KEY.


             10 PI-DRKI1057-BVLAUFENDENUMMER   PIC S9(9) COMP.
             10 PI-DRKI1057-BVPERSONENID   PIC X(13).
             

           05  PI-DRKI1057-DATEN.


             10 PI-DRKI1057-BVBANKBLZID   PIC S9(9) COMP.
             10 PI-DRKI1057-NI-BVBANKBLZID   PIC S9(4) COMP.
             10 PI-DRKI1057-BVINSTITUTBLZ   PIC S9(9) COMP.
             10 PI-DRKI1057-NI-BVINSTITUTBLZ   PIC S9(4) COMP.
             10 PI-DRKI1057-BVINSTITUTNAME   PIC X(50).
             10 PI-DRKI1057-BVKONTOARTID   PIC S9(4) COMP.
             10 PI-DRKI1057-NI-BVKONTOARTID   PIC S9(4) COMP.
             10 PI-DRKI1057-BVKONTONUMMER   PIC X(15).
             10 PI-DRKI1057-BVPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI1057-N-BVPRTGSLLSCHD   PIC S9(4) COMP.
             10 PI-DRKI1057-LETZTEAENDERUNG   PIC X(26).
             
      *
       01  PO-DRKI1057.
           05  PO-DRKI1057-RC                 PIC  X(003).
               88 PO-DRKI1057-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1057-RC-NOK                       VALUE 'NOK'.
      *
           05  PO-DRKI1057-DATEN.
               10 PO-DRKI1057-PTRTABLENAME.                                     
                  49 PO-DRKI1057-PTRTABLENAME-LEN   PIC S9(4) COMP.             
                  49 PO-DRKI1057-PTRTABLENAME-TEXT  PIC X(40).                  
               10 PO-DRKI1057-PTRKEYDATA.                                       
                  49 PO-DRKI1057-PTRKEYDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1057-PTRKEYDATA-TEXT    PIC X(200).                 
               10 PO-DRKI1057-PTRSQLDATA.                                       
                  49 PO-DRKI1057-PTRSQLDATA-LEN     PIC S9(4) COMP.             
                  49 PO-DRKI1057-PTRSQLDATA-TEXT    PIC X(3600).                
      *
      ******* ENDE COPY-MEMBER **************************************
