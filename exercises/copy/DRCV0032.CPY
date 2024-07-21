      *****************************************************************
      *            COPY-MEMBER: DRCV0032                              *
      *                                                               *
      *                                                               *
      *            Inhalt    :  Parameter für Rechnung CO2-neutral    *
      *            Datei     :  CV018100                              *
      *            Member    :  DRCV0032                              *
      *            Organisat.:  VSAM                                  *
      *            Satzl„nge :  38 Byte                               *
      *            Key       :  VBNR,Jahr,Monat (13 Byte)             *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * VLA-4816   !23.05.22! neu                          !J.Hering  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01  DRCV0032-SATZ.
           05 DRCV0032-KEY.
              10 DRCV0032-VBNR                     PIC 9(007).
              10 DRCV0032-JAHR                     PIC 9(004).
              10 DRCV0032-MONAT                    PIC 9(002).
           05 DRCV0032-NETTO-BETRAG                PIC 9(013)V99.
           05 DRCV0032-BRUTTO-BETRAG               PIC 9(013)V99.
           05 DRCV0032-RECH-NR                     PIC 9(010).
      *---------------------- ENDE DRCV0032 --------------------------* 
      *****************************************************************
