      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0851.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0851.CPv  $
      *
      *   Rev 2.1   May 07 1999 15:02:10   T13EX40
      *Funktionserweiterung
      *
      *   Rev 2.0   Apr 29 1999 13:56:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
       01  PI-DRKI0851.                                                         
           05 PI-DRKI0851-LEN                   PIC 9(3).                       
           05 PI-DRKI0851-BINARY                PIC X(256).                     
                                                                                
       01  PO-DRKI0851.                                                         
           05 PO-DRKI0851-RC                    PIC X(3).                       
              88 PO-DRKI0851-RC-OK                VALUE 'OK '.                  
              88 PO-DRKI0851-RC-NOK               VALUE 'NOK'.                  
           05 PO-DRKI0851-STRING                PIC X(512).                     
