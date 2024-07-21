      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0082.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0082.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 28 1996 16:05:24   T13AE26
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0082, TABELLE ALLER BD IM SYSTEM         *
      *    --------------------------------------------------------  *
      *    ERSTELLT: GERLACH                                         *
      *    DATUM:    19.09.1995                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FEVB0286!03.11.95! 01 ! NEU                       ! GERLACH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   BESCHREIBUNG DER TABELLE:
      *   ABGEBILDET WIRD FOLGENDE BEZIEHUNG:
      *     BD <-> D
      *      D <-> ED
      *   IN ZWEI SCHRITTEN. DIE BEZIEHUNG ENSPRICHT
      *   UNTERSTELLTER   <--> BETREUER
      *    ZK-V-VB-UNTST  <--> ZK-V-VB-BETR
      *
      *   DIE SONDERBEHANDLUNG WIRD DURCH EIN ATTRIBUT ERMÖGLICHT,
      *   WELCHES DIESER BEZIEHUNG ZUR IDENTIFIZIERUNG BEIGESTELLT
      *   WIRD. DER EINSTIG ERFOLGT DURCH DAS ATTRIBUT 0, DER WEG
      *   ÜBER DEN BD WIRD DURCH EINE ZAHL > 0 CHARAKTERISIERT.
      *   D.H., JEDER BD ERFORDERT MIND. 2 ZEILEN IN DER TABELLE
      *
      ***************************************************************
       01 Z-BD-VALUES.
          05 ZK-V-TAB-VALUE.
      *             **ADOLF - BUTTER**
             10 ZK-V-VB-UNTST.
                15                  PIC  S9(9)     COMP VALUE 8720070.
                15                  PIC  S9(4)     COMP VALUE 0.
             10 ZK-V-VB-BETR.
                15                  PIC  S9(9)     COMP VALUE 8721000.
                15                  PIC  S9(4)     COMP VALUE 1.
      *             **BUTTER - DOMANETZKY**
             10 ZK-V-VB-UNTST.
                15                  PIC  S9(9)     COMP VALUE 8721000.
                15                  PIC  S9(4)     COMP VALUE 1.
             10 ZK-V-VB-BETR.
                15                  PIC  S9(9)     COMP VALUE 8701700.
                15                  PIC  S9(4)     COMP VALUE 0.
       01 Z-BD-TABELLE REDEFINES Z-BD-VALUES.
          05 ZK-TAB OCCURS 2 INDEXED BY ZK-TAB-IND.
             10 ZK-VB-UNTST.
                15 ZK-UNTST       PIC  S9(9)     COMP.
                15 ZK-UNTST-ATTR  PIC  S9(4)     COMP.
             10 ZK-VB-BETR.
                15 ZK-BETR        PIC  S9(9)     COMP.
                15 ZK-BETR-ATTR   PIC  S9(4)     COMP.
      *
      ******** ENDE COPY-MEMBER *************************************

