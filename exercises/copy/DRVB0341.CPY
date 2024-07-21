      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0341.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0341.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Dec 1998 11:37:02   T13AE28
      *AOP00161 - Neu
      *
      *   Rev 1.0   27 Oct 1998 16:53:24   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0341                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0341                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    11.11.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !11.11.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0341
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    FUNKTION:
      *            ERSTELLEN DES KOMPLEMENTäRKEYS FüR DIE OPAL-DATEI
      *            P13BP06.UU052000.I1.CL  (COPY UU520S)
      *            P13BP06.UU058300.I1.CL  (COPY UU520S)
      *            P13BP06.OP001500.I1.CL  (COPY DROP0005)
      ****************************************************************
      *
       01  PI-DRVB0341.
           05  PI-DRVB0341-JAHR-1             PIC  9(006).
      *
       01  PO-DRVB0341.
           05  PO-DRVB0341-RC                 PIC  X(003).
               88 PO-DRVB0341-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0341-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0341-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0341-KOMPL-KEY          PIC  9(006).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
